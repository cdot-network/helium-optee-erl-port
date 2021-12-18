#ifndef HELIUM_CMD_H
#define HELIUM_CMD_H 1

typedef enum CmdType {
  GenerateECDSAKeypair = 1,
  ECDSASign = 2,
} CmdType;

int read_cmd(uint8_t *buf);
int write_cmd(uint8_t *buf, int len);
int read_exact(uint8_t *buf, int len);
int write_exact(uint8_t *buf, int len);
  
#endif /* HELIUM_CMD_H */
