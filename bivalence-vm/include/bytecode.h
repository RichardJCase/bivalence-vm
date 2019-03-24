#pragma once

#include "int.h"

typedef uint8_t byte;
typedef uint64_t instruction;
typedef uint64_t address;
typedef u8 register_number;

bool read_page(size_t page_start, size_t *instructions_read);
bool read_page_bytes(size_t page_start, byte *bytes, size_t size);
bool write_page_bytes(size_t page_start, byte *bytes, size_t size);

extern byte bytecode_page[PAGE_SIZE];
