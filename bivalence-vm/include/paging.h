#pragma once

#include "common.h"

size_t read_page(size_t page_start);
bool read_page_bytes(size_t page_start, byte *bytes, size_t size);
bool write_page_bytes(size_t page_start, byte *bytes, size_t size);
void load_page(cpu *cpu, size_t page_number);
