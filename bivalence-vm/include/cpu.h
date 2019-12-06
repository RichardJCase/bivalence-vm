#pragma once

#include "int.h"

typedef struct {
  bool br;
  u8 rr;
  size_t rp, sp, bp, ip, ebp;

  i16 sr[NUM_GP_REG];
  u16 ur[NUM_GP_REG];
  float fr[NUM_GP_REG];
  
  byte *execution_stack;
} cpu;

extern cpu cores[MAX_THREADS];

#define main_core cores[0]
