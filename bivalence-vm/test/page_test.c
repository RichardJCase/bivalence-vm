#include <assert.h>
#include "cpu.h"
#include "paging.h"
#include "page_test.h"

#define PROGRAM_CODE ""

static void replacement_test(void){
  for(size_t i = 0; i < NUM_PAGE; i++)
    assert(read_page(PAGE_SIZE * i, NULL));

  
}

static void read_page_test(void){
  cpu cpu = {0};
  byte buffer[sizeof(PROGRAM_CODE)] = {0};
  
  assert(read_page(0, NULL));
  load_page(&cpu, 0);
  assert(cpu.execution_stack);
  
  assert(!memcmp(cpu.execution_stack, PROGRAM_CODE, sizeof(PROGRAM_CODE)));
  assert(read_page_bytes(0, buffer, sizeof(PROGRAM_CODE)));
  assert(!memcmp(buffer, PROGRAM_CODE, sizeof(PROGRAM_CODE)));
}

static void write_page_test(void){
  byte expected[5] = "test";
  byte buffer[5] = {0};

  assert(write_page_bytes(0, expected, sizeof(expected)));
  buffer[0] = 0;
  assert(read_page_bytes(0, buffer, sizeof(buffer)));
  assert(!strncmp((char*)expected, (char*)buffer, sizeof(expected)));
}

void page_test(void){
  replacement_test();
  read_page_test();
  write_page_test();
}
