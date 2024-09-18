#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <stdint.h>
#include <limits.h>

int file_write(int fd, char* a, int al) {
  while (al) {
    ssize_t w = write(fd, a, al);
    if (w == -1)
      return 1;
    a += w;
    al -= w;
  }
  return 0;
}

int file_read(int fd, char* a, int al) {
  while (al) {
    ssize_t r = read(fd, a, al);
    if (r == -1)
      return 1;
    if (r == 0)
      return 2;
    a += r;
    al -= r;
  }
  return 0;
}

int file_open(char* path) {
  int fd = open(path, O_RDWR);
  return fd;
}

// TODO i guess it should fail if off_t < uint64_t
int file_seek(int fd, uint64_t pos) {
  if ((off_t)-1 == lseek(fd, pos, SEEK_SET)) {
    return 1;
  }
  return 0;
}
