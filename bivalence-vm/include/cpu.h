#pragma once

#include "int.h"

typedef uint8_t byte;

typedef struct _cpu {
  bool br;
  u8 rr;
  size_t rp, sp, bp, ip;

  i16 sr[NUM_GP_REG];
  u16 ur[NUM_GP_REG];
  float fr[NUM_GP_REG];
  
  byte *execution_stack;
} cpu;

extern cpu cores[MAX_THREADS];

#define main_core cores[0]
