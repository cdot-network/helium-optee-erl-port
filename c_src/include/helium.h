#ifndef HELIUM_H
#define HELIUM_H 1

__BEGIN_DECLS

/*
  return: 
    1: initialized,
    0: already initialized,
   -1: failed to initialized
Currently, program will quit when initialized failed
 
*/
int helium_init();

/*
  return: 
    1: deinitialized,
    0: not initialized,
*/

int helium_deinit();

/*
  generate key
*/

int gen_ecdsa_keypair();
int ecdsa_sign(void* inbuf, size_t inbuf_len, void* outbuf, size_t* outbuf_len);
__END_DECLS

#endif /* HELIUM_H */
