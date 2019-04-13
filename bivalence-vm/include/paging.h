#pragma once

#include "common.h"

#if PRA == MMAP
byte *program;
#else
FILE *program;
#endif

bool read_page(size_t page_start, size_t *page_index);
bool read_page_bytes(size_t page_start, byte *bytes, size_t size);
bool write_page_bytes(size_t page_start, byte *bytes, size_t size);
void load_page(cpu *cpu, size_t page_number);
bool next_page(cpu *cpu);
