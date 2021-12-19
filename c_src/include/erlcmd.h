#ifndef ERLCMD_H
#define ERLCMD_H 1

#define READ_FD 3
#define WRITE_FD 4

__BEGIN_DECLS

struct erlcmd_buffer;
typedef void (*erlcmd_handler_fp)(struct erlcmd_buffer *pbuf);

struct erlcmd_buffer {
  uint8_t *pbuf;
  size_t bufsz;
  uint32_t pos;
  erlcmd_handler_fp handler;
};

void erlcmd_read_buffer(struct erlcmd_buffer *p_cmd_buffer);

__END_DECLS

#endif // ERLCMD_H
