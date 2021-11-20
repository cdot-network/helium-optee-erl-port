#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <limits.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <err.h>

#include <ei.h>

#include "helium.h"

#define READ_FD 3
#define WRITE_FD 4

typedef unsigned char byte;

int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);

static void usage(int argc, char *argv[])
{
	const char *pname = "acipher";

	if (argc)
		pname = argv[0];

	fprintf(stderr, "usage: %s <key_size> <string to encrypt>\n", pname);
	exit(1);
}

static void get_args(int argc, char *argv[], size_t *key_size, void **inbuf,
		     size_t *inbuf_len)
{
	char *ep;
	long ks;

	if (argc != 3) {
		warnx("Unexpected number of arguments %d (expected 2)",
		      argc - 1);
		usage(argc, argv);
	}

	ks = strtol(argv[1], &ep, 0);
	if (*ep) {
		warnx("cannot parse key_size \"%s\"", argv[1]);
		usage(argc, argv);
	}
	if (ks < 0 || ks == LONG_MAX) {
		warnx("bad key_size \"%s\" (%ld)", argv[1], ks);
		usage(argc, argv);
	}
	*key_size = ks;

	*inbuf = argv[2];
	*inbuf_len = strlen(argv[2]);
}

int main(int argc, char *argv[]) {
  byte buf[100] = {0};
  int fn, arg, res;

  if(argc > 1) {
    size_t key_size;
	void *inbuf;
	size_t inbuf_len;
	size_t n;

    get_args(argc, argv, &key_size, &inbuf, &inbuf_len);
  }
  
  /* printf("start \n"); */
  
  while(read_cmd(buf) > 0) {
    fn = buf[0];
    arg = buf[1];

    /* printf("read_cmd(%d, %d)\n", fn, arg); */

    switch(fn) {
    case 1:
      res = 10;
      break;
    case 2:
    default:
      res = 20;
      break;
    }

    buf[0] = res & 0xff;
    write_cmd(buf, 1);
  }
  return 0;
}

int read_cmd(byte *buf) {
  int len;

  if(read_exact(buf, 2) != 2) {
    return -1;
  }

  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_cmd(byte *buf, int len) {
  int i;
  printf("write_cmd:len:%d\n", len);
  byte li;
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

int read_exact(byte *buf, int len) {
  int i, got = 0;
  do {
    if((i = read(READ_FD, buf+got, len-got)) <= 0) {
      return i;
    }

    got += i;
  } while(got < len);

  return len;
}

int write_exact(byte *buf, int len) {
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
