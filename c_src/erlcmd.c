#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <err.h>
#include <errno.h>
#include <string.h>
#include <arpa/inet.h>

#include "erlcmd.h"

static size_t erlcmd_try_dispatch(struct erlcmd_buffer *p_cmd_buffer);

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

  if (p_cmd_buffer->handler) {
    p_cmd_buffer->handler(p_cmd_buffer);
  }
  
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

