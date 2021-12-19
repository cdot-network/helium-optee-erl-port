#ifndef HELIUM_CMD_H
#define HELIUM_CMD_H 1

__BEGIN_DECLS

typedef enum CmdType {
  GenerateECDSAKeypair = 1,
  ECDSASign = 2,
} CmdType;

// block (while) calling
void helium_handle_erlcmd();
int read_cmd(uint8_t *buf);
int write_cmd(uint8_t *buf, int len);
int read_exact(uint8_t *buf, int len);
int write_exact(uint8_t *buf, int len);

__END_DECLS
  
#endif /* HELIUM_CMD_H */
