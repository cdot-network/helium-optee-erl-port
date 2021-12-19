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
#include "helium.h"

#define DEBUG
#ifdef DEBUG
#define debug(...) do { fprintf(stderr, __VA_ARGS__); fprintf(stderr, "\r\n"); } while(0)
#else
#define debug(...)
#endif

/*
 * https://stackoverflow.com/questions/3437404/min-and-max-in-c
 * https://gcc.gnu.org/onlinedocs/gcc-3.4.6/gcc/Min-and-Max.html
 */
#define MAX(a,b) \
  ({ __typeof__ (a) _a = (a); \
  __typeof__ (b) _b = (b); \
  _a > _b ? _a : _b; })

#define MIN(a,b) \
  ({ __typeof__ (a) _a = (a); \
  __typeof__ (b) _b = (b); \
  _a > _b ? _b : _a; })

#define CMD_BUFFER_SIZE 1024

static void helium_cmd_handler(struct erlcmd_buffer *p_cmd_buffer);
static void helium_gen_ecdsa_keypair(const char *pbuf);
static void helium_ecdsa_sign(const char *pbuf);
static void helium_err_reply_send(const char *cmd, const char *msg);

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
    helium_gen_ecdsa_keypair(p_cmd_buffer->pbuf + buf_idx);
    
  } else if (strcmp(cmd, "ecdsa_sign") == 0) {
    helium_ecdsa_sign(p_cmd_buffer->pbuf + buf_idx);
  } else {
    helium_err_reply_send(cmd, "not supported");
  }
}

void helium_gen_ecdsa_keypair(const char *pbuf) {
  int r = gen_ecdsa_keypair();
  if (0 == r) {
    char resp[256];
    int resp_idx = sizeof(uint16_t) + 1; // Payload length + 'Tag' byte
    resp[2] = 0; // Reply
    ei_encode_version(resp, &resp_idx);
    /* ei_encode_atom(resp, &resp_idx, "reply"); */
    ei_encode_long(resp, &resp_idx, 1);

    erlcmd_send(resp, resp_idx);
  }
  else {
    helium_err_reply_send("gen_ecdsa_keypair", "execution failed");
  }
}

void helium_ecdsa_sign(const char *pbuf) {
  helium_err_reply_send("ecdsa_sign", "not implemented");
}

void helium_err_reply_send(const char *cmd, const char *msg) {
  char resp[256];
  int resp_idx = sizeof(uint16_t) + 1; // Payload length + 'Tag' byte
  resp[2] = 0; // Reply

  ei_encode_version(resp, &resp_idx);

  // format: {err, {cmd, Msg}}
  ei_encode_tuple_header(resp, &resp_idx, 2);
  ei_encode_atom(resp, &resp_idx, "err");

  ei_encode_tuple_header(resp, &resp_idx, 2);
  ei_encode_atom(resp, &resp_idx, cmd);
  const size_t msg_len = strlen(msg);
  const size_t left_len = sizeof(resp) - resp_idx - 1 /* tag */ ;
  if (msg_len > left_len) {
    fprintf(stderr, "%s: msg is too long\n", __FILE__);
  }
  ei_encode_string_len(resp, &resp_idx, msg, MIN(msg_len, left_len));

  erlcmd_send(resp, resp_idx);
}
