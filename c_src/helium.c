#include <err.h>
#include <sys/cdefs.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <limits.h>

/* OP-TEE TEE client API (built by optee_client) */
#include <tee_client_api.h>

/* To the the UUID (found the the TA's h-file(s)) */
#include <helium_optee_ta.h>

#include "helium.h"

static int8_t initialized = 0;
static TEEC_Context ctx;
static TEEC_Session sess;
static TEEC_Operation op;
static TEEC_UUID uuid = TA_HELIUM_UUID;

static void teec_err(TEEC_Result res, uint32_t eo, const char *str)
{
	errx(1, "%s: %#" PRIx32 " (error origin %#" PRIx32 ")", str, res, eo);
}
int8_t helium_init() {
  uint32_t err_origin;
  TEEC_Result res;

  if(initialized) {
    return 0;
  }

  /* Initialize a context connecting us to the TEE */
  res = TEEC_InitializeContext(NULL, &ctx);
  if (res != TEEC_SUCCESS)
    errx(1, "TEEC_InitializeContext failed with code 0x%x", res);

  /*
   * Open a session to the "hello world" TA, the TA will print "hello
   * world!" in the log when the session is created.
   */
  res = TEEC_OpenSession(&ctx, &sess, &uuid,
                         TEEC_LOGIN_PUBLIC, NULL, NULL, &err_origin);
  if (res != TEEC_SUCCESS)
    errx(1, "TEEC_Opensession failed with code 0x%x origin 0x%x",
         res, err_origin);

  initialized = 1;
  return 1;
}

int8_t helium_deinit() {
  if(!initialized) {
    return 0;
  }
  
  /*
   * We're done with the TA, close the session and
   * destroy the context.
   *
   * The TA will print "Goodbye!" in the log when the
   * session is closed.
   */

  TEEC_CloseSession(&sess);

  TEEC_FinalizeContext(&ctx);

  initialized = 0;
  
  return 1;
}

void helium_thread_worker() {
	uint32_t err_origin;
    TEEC_Result res;
	size_t key_size;
	void *inbuf;
	size_t inbuf_len;
	size_t n;

    /*
     * Execute a function in the TA by invoking it, in this case
     * we're incrementing a number.
     *
     * The value of command ID part and how the parameters are
     * interpreted is part of the interface provided by the TA.
     */
	/* Clear the TEEC_Operation struct */
	memset(&op, 0, sizeof(op));

	/*
	 * Prepare the argument. Pass a value in the first parameter,
	 * the remaining three parameters are unused.
	 */
	op.paramTypes = TEEC_PARAM_TYPES(TEEC_VALUE_INOUT, TEEC_NONE,
					 TEEC_NONE, TEEC_NONE);
	op.params[0].value.a = 42;

	/*
	 * TA_HELLO_WORLD_CMD_INC_VALUE is the actual function in the TA to be
	 * called.
	 */
	printf("Invoking TA to increment %d\n", op.params[0].value.a);
	res = TEEC_InvokeCommand(&sess, TA_HELIUM_CMD_INC_VALUE, &op,
				 &err_origin);
	if (res != TEEC_SUCCESS)
		errx(1, "TEEC_InvokeCommand failed with code 0x%x origin 0x%x",
			res, err_origin);
	printf("TA incremented value to %d\n", op.params[0].value.a);


}
