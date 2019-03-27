#pragma once

#include "common.h"
#include "cpu.h"

typedef struct _cpu {
  bool br;
  u8 rr;
  size_t rp, sp, bp, ip;

  i16 sr[NUM_GP_REG];
  u16 ur[NUM_GP_REG];
  float fr[NUM_GP_REG];
  
  byte execution_stack[PAGE_SIZE];
} cpu;

extern cpu cores[MAX_THREADS];

#define main_core cores[0]

bool load_page(cpu *core);
