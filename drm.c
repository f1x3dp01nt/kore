#include <stdbool.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <libkms.h>
#include <xf86drm.h>
#include <xf86drmMode.h>
#include <drm_fourcc.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

size_t iconn;
size_t imode;
unsigned fb_pitch;
uint16_t fb_height;
uint16_t fb_width;
void *fb_mem;
void *fb_buffer;
bool fb_inited;
int fd=-1;
struct kms_driver* kms;
struct kms_bo* kms_bo;

uint16_t fb_get_width() { return fb_width; }
uint16_t fb_get_height() { return fb_height; }

int32_t fb_clear(uint32_t c) {
  if (!fb_inited) return -1;
  uint32_t (*fb)[fb_pitch/4] = fb_buffer;
  for (size_t y=0; y<fb_height; y++)
    for (size_t x=0; x<fb_width; x++)
      fb[y][x] = c;
  return 0;
}

int32_t fb_fillrect(uint32_t x, uint32_t y, uint32_t w, uint32_t h, uint32_t c) {
  if (!fb_inited) return -1;
  uint64_t xl=x, yl=y, wl=w, hl=h;
  if (xl+wl > fb_width)
    w = x >= fb_width ? 0 : fb_width-1 - x;
  if (yl+hl > fb_height)
    h = y >= fb_height ? 0 : fb_height-1 - y;
  uint32_t (*fb)[fb_pitch/4] = fb_buffer;
  for (size_t yo =0; yo<h; yo++)
    for (size_t xo = 0; xo<w; xo++)
      fb[y+yo][x+xo] = c;
  return 0;
}

int32_t fb_wait_vblank() {
  if (!fb_inited) return -1;
  // XXX how do we know when to use DRM_VBLANK_SECONDARY?
  drmVBlankReq vbr = {
    .type=DRM_VBLANK_RELATIVE | DRM_VBLANK_NEXTONMISS,
    .sequence=0,
    .signal=0
  };
  drmVBlank vb;
  vb.request = vbr;
  drmWaitVBlank(fd, &vb);
  return 0;
}

void fb_copy() {
  if (!fb_inited) return;
  memcpy(fb_mem, fb_buffer, fb_height*(fb_pitch/4)*4);
  /* This writes outside the framebuffer (the right column created by the "pitch").
   * Shouldn't be a problem but who knows. */
}

/* Shut all monitors and setup a framebuffer to be displayed on one of them, decided by the index `iconn`.
 * The monitor is modeswitched to its preferred modeline
 * If there are no preferred modelines, the first in the list is used.
 * If there is more than one preferred modeline (it can happen), the first one in the list is used.
 * What is "the list"? Something we get from DRM-KMS API. It seems to put highest resolutions first. 
 * Hopefully it doesn't put broken highest resolutions higher than working resolutions.
 */
int32_t fb_init() {
  drmModeResPtr res = NULL;
  drmModeConnectorPtr conn = NULL;
  drmModeCrtcPtr crtc = NULL;

  if (fb_inited) {
    /* XXX can the "bo" be destroyed before disassociating it from the CRTC, connector, etc?
     * API doesn't say anything, and it worked on my system
     * on that matter i dont know which order any stuff has to be uninitialized */
    kms_bo_destroy(&kms_bo);
    kms_destroy(&kms);
    free(fb_buffer);
    fb_buffer = NULL;
  }

  int32_t ret = -1;
  // XXX I don't know how you're actually meant to choose a card so just using this for now. it might even violate libDRM API
  if (fd == -1) {
    if ((fd = open("/dev/dri/card0", O_RDWR)) == -1) {
      printf("open fail\n");
      goto out;
    }
  }

  if ((res = drmModeGetResources(fd)) == NULL) {
    printf("getres fail\n");
    goto out;
  }

  if (!res->count_connectors) {
    printf("no connectors\n");
    goto out;
  }

  // turn off all CRTCs. XXX No idea how to do this properly. just setting everything to NULL worked on my system
  for (size_t i=0;i<res->count_crtcs;i++) {
    if (drmModeSetCrtc(fd, res->crtcs[i], 0, 0, 0, NULL, 0, NULL)) {
      printf("failed to disable CRTC %" PRIu32 "\n", res->crtcs[i]);
      goto out;
    }
  }

  if (iconn >= res->count_connectors) iconn = 0;
  if ((conn = drmModeGetConnector(fd, res->connectors[iconn])) == NULL) {
    printf("failed to get connector\n");
    goto out;
  }
  iconn++;
  // TODO maybe handle stuff with DRM_MODE_UNKNOWNCONNECTION
  if (conn->connection!=DRM_MODE_CONNECTED) {
    printf("connector %" PRIu32 " not connected\n", conn->connector_id);
    goto out;
  }
  if (!conn->count_modes) {
    printf("connector has %" PRIu32 "no modes\n", conn->connector_id);
    goto out;
  }

  bool got_pref_mode = false;
  for (imode=0; imode<conn->count_modes; imode++) {
    if (conn->modes[imode].type & DRM_MODE_TYPE_PREFERRED) {
      got_pref_mode=true;
      break;
    }
  }
  if (!got_pref_mode) {
    printf("connector has no preferred mode\n");
    imode = 0;
  }

  if (kms_create(fd, &kms)) {
    printf("kms_create fail\n");
    kms=NULL;
    goto out;
  }
  unsigned attr[] = {
    KMS_WIDTH, conn->modes[imode].hdisplay,
    KMS_HEIGHT, conn->modes[imode].vdisplay,
    KMS_BO_TYPE, KMS_BO_TYPE_SCANOUT_X8R8G8B8,
    KMS_TERMINATE_PROP_LIST
  };
  if (kms_bo_create(kms, attr, &kms_bo)) {
    printf("kms_bo_create fail\n");
    kms_bo=NULL;
    goto out;
  }
  if (kms_bo_map(kms_bo, &fb_mem)) {
    printf("kms_bo_map fail\n");
    goto out;
  }
  unsigned handle;
  if (kms_bo_get_prop(kms_bo, KMS_PITCH, &fb_pitch)) {
    printf("kms_bo_get_prop KMS_PITCH fail\n");
    goto out;
  }
  if (kms_bo_get_prop(kms_bo, KMS_HANDLE, &handle)) {
    printf("kms_bo_get_prop KMS_HANDLE fail\n");
    goto out;
  }

  uint32_t buf_id;
  uint32_t handles[]={handle, 0, 0, 0},
           pitches[]={fb_pitch, 0, 0, 0},
           offsets[]={0, 0, 0, 0};
  if (drmModeAddFB2(
    fd,
    conn->modes[imode].hdisplay,
    conn->modes[imode].vdisplay,
    DRM_FORMAT_XRGB8888,
    handles,
    pitches,
    offsets,
    &buf_id,
    0
  )) {
    printf("drmModeAddFB2 fail\n");
    goto out;
  }

  if ((crtc = drmModeGetCrtc(fd, res->crtcs[0])) == NULL) {
    printf("drmModeGetCrtc failed\n");
    goto out;
  }
  uint32_t connectors[] = { conn->connector_id };
  if (drmModeSetCrtc(fd, crtc->crtc_id, buf_id, 0, 0, connectors, 1, &conn->modes[imode])) {
    printf("drmModeSetCrtc fail\n");
    goto out;
  }
  if (fb_pitch % 4) {
    printf("pitch (%u) not divisible by 4. can't handle this\n", fb_pitch);
    goto out;
  }
  fb_width = conn->modes[imode].hdisplay;
  fb_height = conn->modes[imode].vdisplay;

  fb_buffer = malloc(4*(fb_pitch/4)*fb_height);
  if (fb_buffer == NULL) {
    perror("can't malloc fb_buffer");
    goto out;
  }

  fb_inited = 1;
  ret = 0;
  goto out_good;

out:
  fb_inited = 0;
  if (fb_buffer != NULL) free(fb_buffer);
out_good:
  if (res != NULL)       drmModeFreeResources(res);
  if (conn != NULL)      drmModeFreeConnector(conn);
  if (crtc != NULL)      drmModeFreeCrtc(crtc);
  return ret;
}
