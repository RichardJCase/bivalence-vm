#pragma once

#include "int.h"

#define LFU 1
#define LRU 2
#define BOTH 3
#define MMAP 4

#if PRA == MMAP
extern byte *program;
#else
extern FILE *program;
#endif

typedef struct {
  bool written;
  size_t use_id, uses, start;

#if PRA == MMAP
  byte *memory;
#else
  byte memory[PAGE_SIZE];
#endif
} page;

extern page page_table[NUM_PAGE];

/* skips checks for cached pages */
bool read_page(size_t page_start, size_t *page_index);

bool read_page_bytes(size_t page_start, byte *bytes, size_t size);
bool write_page_bytes(size_t page_start, byte *bytes, size_t size);
void load_page(cpu *cpu, size_t page_number);
bool next_page(cpu *cpu);
