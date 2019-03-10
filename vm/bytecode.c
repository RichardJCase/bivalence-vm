#include "common.h"

#include <unistd.h>

static size_t file_position = 0;
byte bytecode_page[PAGE_SIZE] = {0};

bool read_page(size_t page_start, size_t *bytes_read){
  long pos = fseek(program, (long int)page_start, SEEK_CUR);
  if(pos == -1) return false;
  file_position = (size_t)pos;
  
  *bytes_read = fread(bytecode_page, 1, PAGE_SIZE, program);
  memset(bytecode_page + *bytes_read, 0, PAGE_SIZE - *bytes_read);

  return true;
}
