#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <poll.h>
#include <err.h>
#include <errno.h>
#include <unistd.h>

#include "helium_cmd.h"

#define READ_FD 3
#define WRITE_FD 4

#define CMD_BUFFER_SIZE 1024

struct erlcmd_buffer {
  uint8_t *pbuf;
  size_t bufsz;
  uint32_t pos;
};
static size_t erlcmd_try_dispatch(struct erlcmd_buffer *p_cmd_buffer);
static void erlcmd_read_buffer(struct erlcmd_buffer *p_cmd_buffer);

void helium_handle_erlcmd() {
  struct erlcmd_buffer cmd_buffer;
  memset(&cmd_buffer, 0, sizeof(cmd_buffer));
  cmd_buffer.bufsz = CMD_BUFFER_SIZE;
  cmd_buffer.pbuf = (uint8_t *)malloc(CMD_BUFFER_SIZE * sizeof(uint8_t));
  if(NULL == cmd_buffer.pbuf) {
    err(EXIT_FAILURE, "out of memory");
  }
  while(1) {
    struct pollfd fdset[1];
    fdset[0].fd = READ_FD;
    fdset[0].events = POLLIN;
    fdset[0].revents = 0;

    int rc = poll(fdset, sizeof(fdset) / sizeof(fdset[0]), -1);
    if (rc < 0) {
      // /Retry if error is EINTR
      if (errno == EINTR)
        continue;
      err(EXIT_FAILURE, "poll");
    }

    if (fdset[0].revents & (POLLIN | POLLHUP)) {
      erlcmd_read_buffer(&cmd_buffer);
    }
  }

  if(cmd_buffer.pbuf) {
    free(cmd_buffer.pbuf);
  }
}

size_t erlcmd_try_dispatch(struct erlcmd_buffer *p_cmd_buffer) {
  uint16_t be_len, msg_len;
  
  /* Check for length field */
  if (p_cmd_buffer->pos < sizeof(be_len)) {
    return 0;
  }

  memcpy(&be_len, p_cmd_buffer->pbuf, sizeof(be_len));
  msg_len = ntohs(be_len);
  if (msg_len + sizeof(msg_len) > p_cmd_buffer->bufsz) {
    errx(EXIT_FAILURE, "Message is too long");
  }
  
  /* Message is not complete */
  if (msg_len + sizeof(msg_len) > p_cmd_buffer->pos) {
    return 0;
  }

  /* handler */
  
  return msg_len + sizeof(msg_len);
}

void erlcmd_read_buffer(struct erlcmd_buffer *p_cmd_buffer) {
  size_t r = read(READ_FD, p_cmd_buffer->pbuf + p_cmd_buffer->pos, p_cmd_buffer->bufsz - p_cmd_buffer->pos);
  if (r < 0) {
    /* ignore EINTR */
    if (errno == EINTR) return ;

    err(EXIT_FAILURE, "read");
  } else if (0 == r) {
    /* EOF. */
    exit(EXIT_SUCCESS);
  }

  if(p_cmd_buffer->pos + r > p_cmd_buffer->bufsz) {
    err(EXIT_FAILURE, "cmd_buffer is too small");
  }
  // successfully read data
  p_cmd_buffer->pos += r;
  while(1) {
    size_t processed = erlcmd_try_dispatch(p_cmd_buffer);
    if (0 == processed) {
      break;
    } else if (p_cmd_buffer->pos > processed) {
      /* 
       * processed part of buffer, but have more data left,
       * then clear out processed buffer, and move left 
       * buffer to front
       * this should be optimized by minimize memmove
       */
      p_cmd_buffer->pos -= processed;
      memmove(p_cmd_buffer->pbuf,
              p_cmd_buffer->pbuf + processed,
              p_cmd_buffer->pos);
    } else {
      /* All buffer has been processed */
      p_cmd_buffer->pos = 0;
      break;
    }
  }
}

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
