#pragma once

typedef enum {
  UNINIT,
  POKE_SR,
  POKE_UR,
  POKE_FR,
  PEEK_SR,
  PEEK_UR,
  PEEK_FR,

  ADD_SR,
  ADD_UR,
  ADD_FR,
  SUB_SR,
  SUB_UR,
  SUB_FR,
  MUL_SR,
  MUL_UR,
  MUL_FR,
  DIV_SR,
  DIV_UR,
  DIV_FR,
  AND_SR,
  AND_UR,
  OR_SR,
  OR_UR,
  XOR_SR,
  XOR_UR,

  MOV_SR_SR,
  MOV_SR_UR,
  MOV_SR_FR,
  MOV_UR_SR,
  MOV_UR_UR,
  MOV_UR_FR,
  MOV_FR_SR,
  MOV_FR_UR,
  MOV_FR_FR,
  MOV_SR_N,
  MOV_UR_N,
  MOV_FR_N,

  CALL_ADDR,
  CALL_UR,
  JMP_ADDR,
  JMP_UR,
  RET,
  STOP,
  
  LOAD,
  SYM,
  CCALL
} opcode;

#define REG_MASK 0x1F
#define ADDR_MASK 0x1FFFFFFFFFFFF
#define VALUE_MASK ADDR_MASK
