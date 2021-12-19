#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <poll.h>
#include <err.h>
#include <errno.h>

#include <ei.h>

#include "erlcmd.h"
#include "helium_cmd.h"

#define DEBUG
#ifdef DEBUG
#define debug(...) do { fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\r\n"); } while(0)
#else
#define debug(...)
#endif

#define CMD_BUFFER_SIZE 1024

static void helium_cmd_handler(struct erlcmd_buffer *p_cmd_buffer);

void helium_handle_erlcmd(erlcmd_handler_fp handler) {
  struct erlcmd_buffer cmd_buffer;
  memset(&cmd_buffer, 0, sizeof(cmd_buffer));
  cmd_buffer.bufsz = CMD_BUFFER_SIZE;
  cmd_buffer.pbuf = (char *)malloc(CMD_BUFFER_SIZE * sizeof(char));
  if (handler) {
    cmd_buffer.handler = handler;
  } else {
    cmd_buffer.handler = helium_cmd_handler;
  }

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
      debug("got read buffer");
      erlcmd_read_buffer(&cmd_buffer);
    } else {
      debug("poll got other revents");
    }
  } // while

  if(cmd_buffer.pbuf) {
    free(cmd_buffer.pbuf);
  }
}

void helium_cmd_handler(struct erlcmd_buffer *p_cmd_buffer) {
  int buf_idx = sizeof(uint16_t);
  int version = 0;
  if (ei_decode_version(p_cmd_buffer->pbuf, &buf_idx, &version) < 0) {
    errx(EXIT_FAILURE, "Message version issue");
  }

  int arity = 0;
  if (ei_decode_tuple_header(p_cmd_buffer->pbuf, &buf_idx, &arity) < 0) {
    errx(EXIT_FAILURE, "expecting {cmd, args} tuple");
  }

  char cmd[MAXATOMLEN];
  if (ei_decode_atom(p_cmd_buffer->pbuf, &buf_idx, cmd) < 0) {
    errx(EXIT_FAILURE, "expecting command atom");
  }

  debug("atom command: %s", cmd);

  if (strcmp(cmd, "gen_ecdsa_keypair") == 0) {
    debug("gen_ecdsa_keypair");
  } else if (strcmp(cmd, "ecdsa_sign") == 0) {
    debug("ecdsa_sign");
  }

  char resp[256];
  int resp_idx = sizeof(uint16_t) + 1; // Payload length + 'Tag' byte
  resp[2] = 0; // Reply
  ei_encode_version(resp, &resp_idx);
  /* ei_encode_atom(resp, &resp_idx, "reply"); */
  ei_encode_long(resp, &resp_idx, 1);

  erlcmd_send(resp, resp_idx);
}
/*
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

  // printf("write_exact(%d):0x%x\n", len, buf[0]);
  do {
    if((i = write(WRITE_FD, buf+written, len - written)) <= 0) {
      err(1, "write failed: %d", i);
      return i;
    }
    written += i;
  }while(written < len);

  return len;
}
*/
