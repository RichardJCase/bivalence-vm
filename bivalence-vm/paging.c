#include <time.h>
#include <unistd.h>
#include "common.h"

typedef struct {
  time_t last_use;
  size_t use;
  byte memory[PAGE_SIZE];
} page;

static size_t file_position = 0;
static page page_table[NUM_PAGE] = {0};

//todo: forces a page fault
size_t read_page(size_t page_start){
  long pos = fseek(program, (long int)page_start, SEEK_CUR);
  if(pos == -1) return false;
  file_position = (size_t)pos;

  unused(page_table);
  /* todo: grab memory from next available page
  *bytes_read = fread(bytecode_page, 1, PAGE_SIZE, program);
  memset(bytecode_page + *bytes_read, 0, PAGE_SIZE - *bytes_read);
  */
  
  return 0; //todo: return page in table
}

bool read_page_bytes(size_t page_start, byte *bytes, size_t size){
  unused(page_start);
  unused(bytes);
  unused(size);
  return true; //todo
}

bool write_page_bytes(size_t page_start, byte *bytes, size_t size){
  unused(page_start);
  unused(bytes);
  unused(size);
  return true; //todo
}

void load_page(cpu *core, size_t page_number){
  core->execution_stack = page_table[page_number].memory;
}
