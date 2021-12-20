#include <err.h>
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

static int initialized = 0;
static TEEC_Context ctx;
static TEEC_Session sess;
static TEEC_UUID uuid = TA_HELIUM_UUID;

static void teec_err(TEEC_Result res, uint32_t eo, const char *str)
{
	errx(1, "%s: %#" PRIx32 " (error origin %#" PRIx32 ")", str, res, eo);
}
int helium_init() {
  uint32_t err_origin;
  TEEC_Result res;

  if(initialized) {
    return 1;
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
  return 0;
}

int helium_deinit() {
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
  
  return 0;
}

int gen_ecdsa_keypair() {
  uint32_t err_origin;
  TEEC_Result res;
  TEEC_Operation op;
  const size_t key_size = 256;
  
  memset(&op, 0, sizeof(op));
  op.paramTypes = TEEC_PARAM_TYPES(TEEC_VALUE_INPUT, TEEC_NONE,
                                   TEEC_NONE, TEEC_NONE);
  op.params[0].value.a = key_size;

  res = TEEC_InvokeCommand(&sess, TA_HELIUM_CMD_GEN_ECDSA_KEYPAIR, &op, &err_origin);
  if (res)
    teec_err(res, err_origin, "TEEC_InvokeCommand(TA_HELIUM_CMD_GEN_ECDSA_KEYPAIR)");
  return 0;
}

int ecdsa_sign(void* inbuf, size_t inbuf_len, void* outbuf, size_t* outbuf_len) {
  uint32_t err_origin;
  TEEC_Operation op;
  TEEC_Result res;
  
  memset(&op, 0, sizeof(op));
  op.paramTypes = TEEC_PARAM_TYPES(TEEC_MEMREF_TEMP_INPUT,
                                   TEEC_MEMREF_TEMP_OUTPUT,
                                   TEEC_NONE, TEEC_NONE);
  op.params[0].tmpref.buffer = inbuf;
  op.params[0].tmpref.size = inbuf_len;
  op.params[1].tmpref.buffer = outbuf;
  op.params[1].tmpref.size = *outbuf_len;
  res = TEEC_InvokeCommand(&sess, TA_HELIUM_CMD_ECDSA_SIGN, &op, &err_origin);
  if (res) {
    teec_err(res, err_origin, "TEEC_InvokeCommand(TA_HELIUM_CMD_ECDSA_SIGN)");
    return 1;
  }

  return 0;
}

int gen_ecdh_keypair() {
  uint32_t err_origin;
  TEEC_Result res;
  TEEC_Operation op;
  const size_t key_size = 256;
  
  memset(&op, 0, sizeof(op));
  op.paramTypes = TEEC_PARAM_TYPES(TEEC_VALUE_INPUT, TEEC_NONE,
                                   TEEC_NONE, TEEC_NONE);
  op.params[0].value.a = key_size;

  res = TEEC_InvokeCommand(&sess, TA_HELIUM_CMD_GEN_ECDH_KEYPAIR, &op, &err_origin);
  if (res)
    teec_err(res, err_origin, "TEEC_InvokeCommand(TA_HELIUM_CMD_GEN_ECDH_KEYPAIR)");
  return 0;
}

int ecdh(const void *X, const size_t x_len, const void* Y, const size_t y_len, void *secret, size_t *secret_len) {
  uint32_t err_origin;
  TEEC_Operation op;
  TEEC_Result res;
  
  memset(&op, 0, sizeof(op));
  op.paramTypes = TEEC_PARAM_TYPES(TEEC_MEMREF_TEMP_INPUT,
                                   TEEC_MEMREF_TEMP_INPUT,
                                   TEEC_MEMREF_TEMP_OUTPUT,
                                   TEEC_NONE);
  op.params[0].tmpref.buffer = (void *)X;
  op.params[0].tmpref.size = x_len;
  op.params[1].tmpref.buffer = (void *)Y;
  op.params[1].tmpref.size = y_len;
  op.params[2].tmpref.buffer = secret;
  op.params[2].tmpref.size = *secret_len;

  res = TEEC_InvokeCommand(&sess, TA_HELIUM_CMD_ECDH, &op, &err_origin);
  if (res) {
    teec_err(res, err_origin, "TEEC_InvokeCommand(TA_HELIUM_CMD_ECDH)");
    return 1;
  }

  return 0;
}
