/*
 * Copyright (c) 2016-2017, Qingdao IotPi Information Technology Ltd.
 * All rights reserved.
 *
 */
#ifndef TA_HELIUM_H
#define TA_HELIUM_H


/*
 * This UUID is generated with uuidgen
 * the ITU-T UUID generator at http://www.itu.int/ITU-T/asn1/uuid.html
 */
#define TA_HELIUM_UUID \
	{ 0x755c7d73, 0x41a1, 0x4f9a, \
		{ 0xb8, 0x29, 0x6a, 0x91, 0xb9, 0xfd, 0xf1, 0x09} }

/*
 * in	params[0].value.a key size
 */
#define TA_HELIUM_CMD_GEN_ECDSA_KEYPAIR		2

/*
 * in	params[1].memref  input
 * out	params[2].memref  output
 */
#define TA_HELIUM_CMD_ENCRYPT		        3
/*
 * in  params[0].memref input: message
 * out params[1].memref output: signature
 */
#define TA_HELIUM_CMD_ECDSA_SIGN            4

#define TA_HELIUM_CMD_ECDH                  5

/*
 * in	params[0].value.a key size
 */
#define TA_HELIUM_CMD_GEN_ECDH_KEYPAIR		6

#endif /*TA_HELIUM_H*/
