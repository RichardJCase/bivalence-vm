#include <assert.h>
#include "cpu.h"
#include "paging.h"
#include "page_test.h"

#define PROGRAM_CODE ""

static void read_page_test(void){
  cpu cpu = {0};
  byte buffer[sizeof(PROGRAM_CODE)] = {0};
  
  read_page(0);
  load_page(&cpu, 0);
  assert(cpu.execution_stack);
  
  assert(!memcmp(cpu.execution_stack, PROGRAM_CODE, sizeof(PROGRAM_CODE)));
  assert(read_page_bytes(0, buffer, sizeof(PROGRAM_CODE)));
  assert(!memcmp(buffer, PROGRAM_CODE, sizeof(PROGRAM_CODE)));
}

static void write_page_test(void){
  byte expected = 5;
  byte buffer[1] = {expected};
  
  assert(write_page_bytes(0, buffer, 1));
  buffer[0] = 0;
  assert(read_page_bytes(0, buffer, 1));
  assert(buffer[0] == expected);
}

void page_test(void){  
  read_page_test();
  write_page_test();
}
