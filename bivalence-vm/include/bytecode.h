#pragma once

#include "int.h"

typedef uint8_t byte;

bool read_page(size_t page_start, size_t *bytes_read);
bool read_page_bytes(size_t page_start, byte *bytes, size_t size);
bool write_page_bytes(size_t page_start, byte *bytes, size_t size);

extern byte bytecode_page[PAGE_SIZE];
