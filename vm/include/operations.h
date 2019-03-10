#pragma once

typedef enum {
  UNINIT,
  STOP,
  CALL,
  JMP,
  RET,

  ADD,
  SUB,
  MUL,
  DIV,
  LSL,
  LSR,
  OR,
  XOR,
  AND,
  
  POKE,
  PEEK,

  //IO

  //
} opcode;
