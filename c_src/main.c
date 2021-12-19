#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <limits.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <err.h>

#include "helium_cmd.h"

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
  uint8_t buf[100] = {0};
  int fn, arg, res;

  if(argc > 1) {
    size_t key_size;
	void *inbuf;
	size_t inbuf_len;

    get_args(argc, argv, &key_size, &inbuf, &inbuf_len);
  }

  if(helium_init() < 0) {
    fprintf(stderr, "failed to init helium\n");
    exit(1);
  }

  // this is a block (while(1)) func calling
  helium_handle_erlcmd(NULL);
  
  helium_deinit();
  fprintf(stderr, "%s is exiting...\n", argv[0]);

  return 0;
}

