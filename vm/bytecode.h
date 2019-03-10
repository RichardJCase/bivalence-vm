#pragma once

#include "int.h"

typedef uint8_t byte;

bool read_page(size_t page_start, size_t *bytes_read);

extern byte bytecode_page[PAGE_SIZE];
