#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <sys/ioctl.h>
#include <poll.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <stdbool.h>
#include <linux/input.h>

int mfd;
bool input_inited;
struct pollfd kbds[20]; // arbitrary limit
size_t num_kbds;

/*
 * Return
 * 0 - error
 * 1 - success
 */
int32_t input_init() {
  memset((void*)kbds, 0, sizeof(kbds));
  for (size_t i=0, j=0; i < sizeof(kbds)/sizeof(*kbds); j++) {
    char name[500];  // arbitrary length. clearly big enough for any machine word size
    uint64_t mask = 0; /* XXX copied from old code. don't remember if this is the right size.
                        * let's set it to 0 in case only part of the word is set */
    sprintf(name, "/dev/input/event%zu", j);
    if ((kbds[i].fd = open(name, O_RDONLY | O_NONBLOCK)) == -1)
      break;
    kbds[i].events=POLLIN;
    if (ioctl(kbds[i].fd, EVIOCGBIT(0, sizeof(mask)), &mask) == -1) {
      perror("error doing EVIOCGBIT");
      return 0;
    }
    if (!(mask >> EV_KEY&1)) continue;
    // XXX can't remember if or why these two lines are needed:
    if (mask >> EV_REL&1) continue;
    if (mask >> EV_ABS&1) continue;
    // If the above are needed then I have no idea how you're supposed to know whether something is a keyboard
    num_kbds++;
    i++;
  }
  for (size_t i=0; i < num_kbds; i++)
    if (ioctl(kbds[i].fd, EVIOCGRAB, 1) == -1)
      printf("failed to grab keyboard\n");
  if (!num_kbds) {
    printf("no keyboards\n");
    return 0;
  }
  mfd = open("/dev/input/mice", O_RDONLY | O_NONBLOCK);
  if (mfd == -1) {
    perror("failed to open mice\n");
    return 0;
  }
  input_inited=1;
  return 1;
}

/*
 * Return:
 *  0 - error
 *  1 - got input
 *  2 - no input available
 */
// XXX if multiple keyboards report an event at once, only the first in `kbds` is returned
struct keyboard_ret {
  int32_t status;
  int32_t type;
  int32_t code;
  int32_t value;
};
struct keyboard_ret* input_keyboard() {
  static struct keyboard_ret ret;
  if (!input_inited) {
    ret.status = 0;
    return &ret;
  }
  struct input_event e;
  switch (poll(kbds, num_kbds, 0)) {
    case -1:
      perror("poll failed\n");
      ret.status = 0;
      return &ret;
    case 0:
      ret.status = 2;
      return &ret;
  }
  for (size_t i=0; i < sizeof(kbds)/sizeof(struct pollfd); i++) {
    if (kbds[i].revents & POLLIN) {
      if (read(kbds[i].fd, &e, sizeof(e)) == -1) {
        if (errno == EAGAIN || errno == EWOULDBLOCK)
          continue;
        perror("kbds: unknown read error\n");
        return 0;
      }
      if (e.type != EV_KEY) continue;
      ret.type = e.type;
      ret.code = e.code;
      ret.value = e.value;
      ret.status = 1;
      return &ret;
    }
  }
  ret.status = 2;
  return &ret;
}

void input_unblank() {
  printf("\x1b[13]");
  fflush(stdout);
}

/*
 * status:
 *  0 - error
 *  1 - got input
 *  2 - no input available
 */
struct mouse_ret {
  int32_t status;
  int32_t xd;
  int32_t yd;
  int32_t click;
  int32_t altClick;
};
struct mouse_ret* input_mouse() {
  static struct mouse_ret ret;
  ret.click = 0;
  ret.altClick = 0;
  ret.xd = 0;
  ret.yd = 0;
  if (!input_inited) {
    ret.status = 0;
    return &ret;
  }
  char b[3];
  int nr = read(mfd, &b, 3);
  if (nr == -1) {
    if (errno == EAGAIN || errno == EWOULDBLOCK) {
      ret.status = 2;
      return &ret;
    }
    printf("mfd: unknown read error\n");
    ret.status = 0;
    return &ret;
  }
  if (nr != 3) {
    printf("wrong nr %d\n", nr);
    ret.status = 0;
    return &ret;
  }
  if (b[0] & 0x38) { // XXX constants?
    ret.xd = b[1];
    ret.yd = b[2];
  }
  if (b[0] & 1) // XXX constant?
    ret.click = 1;
  if (b[0] & 2)
    ret.altClick = 1;
  ret.status = 1;
  return &ret;
}
