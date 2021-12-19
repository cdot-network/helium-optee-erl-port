#ifndef HELIUM_CMD_H
#define HELIUM_CMD_H 1

#include "erlcmd.h"

__BEGIN_DECLS

typedef enum CmdType {
  GenerateECDSAKeypair = 1,
  ECDSASign = 2,
} CmdType;

// block (while) calling
void helium_handle_erlcmd(erlcmd_handler_fp handler);

__END_DECLS
  
#endif /* HELIUM_CMD_H */
