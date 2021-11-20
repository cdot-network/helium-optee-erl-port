#ifndef HELIUM_H
#define HELIUM_H 1

__BEGIN_DECLS

void helium_thread_worker();

/*
  return: 
    1: initialized,
    0: already initialized,
   -1: failed to initialized
Currently, program will quit when initialized failed
 
*/
int8_t helium_init();

/*
  return: 
    1: deinitialized,
    0: not initialized,
*/

int8_t helium_deinit();


__END_DECLS

#endif /* HELIUM_H */
