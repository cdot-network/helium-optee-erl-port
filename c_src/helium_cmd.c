#include <stdio.h>
#include <stdint.h>

#include "helium_cmd.h"

#define READ_FD 3
#define WRITE_FD 4

int read_cmd(uint8_t *buf) {
  int len;

  if(read_exact(buf, 2) != 2) {
    return -1;
  }

  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_cmd(uint8_t *buf, int len) {
  int i;
  printf("write_cmd:len:%d\n", len);
  uint8_t li;
  li = (len >> 8) & 0xff;
  if((i = write_exact(&li, 1)) < 0) {
    return i;
  }
  li = (len & 0xff);
  if((i = write_exact(&li, 1)) < 0) {
    return i;
  }
  return write_exact(buf, len);
}

int read_exact(uint8_t *buf, int len) {
  int i, got = 0;
  do {
    if((i = read(READ_FD, buf+got, len-got)) <= 0) {
      return i;
    }

    got += i;
  } while(got < len);

  return len;
}

int write_exact(uint8_t *buf, int len) {
  int i, written = 0;

  /* printf("write_exact(%d):0x%x\n", len, buf[0]); */
  do {
    if((i = write(WRITE_FD, buf+written, len - written)) <= 0) {
      err(1, "write failed: %d", i);
      return i;
    }
    written += i;
  }while(written < len);

  return len;
}
