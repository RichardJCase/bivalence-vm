#include <assert.h>
#include "cpu.h"
#include "paging.h"
#include "page_test.h"
#include "stdio.h"
#include <stdlib.h>
#include <string.h>

static void replacement_test(void){
  for(size_t i = 0; i < NUM_PAGE + 1; i++)
    assert(read_page(0, NULL));

  size_t page_index = NUM_PAGE;
  assert(read_page(0, &page_index));

  for(size_t i = 0; i < NUM_PAGE; i++)
    printf("Uses: %zu\n", page_table[i].uses);

  for(size_t i = 0; i < NUM_PAGE; i++)
    printf("ID: %zu\n", page_table[i].use_id);

  printf("page test: %zu\n", page_index);
  assert(page_index == 1);
}

static void read_page_test(void){
  cpu cpu = {0};
  byte buffer[PAGE_SIZE * NUM_PAGE] = {0};
  byte expected[PAGE_SIZE * NUM_PAGE] = {0};
  
  assert(read_page(0, NULL));
  load_page(&cpu, 0);
  assert(cpu.execution_stack);

  assert(read_page_bytes(0, buffer, sizeof(buffer)));
  assert(!memcmp(buffer, expected, sizeof(expected)));
}

static void write_page_test(void){
  byte expected[5] = "test";
  byte buffer[5] = {0};

  assert(write_page_bytes(0, expected, sizeof(expected)));
  assert(read_page_bytes(0, buffer, sizeof(buffer)));
  assert(!strncmp((char*)expected, (char*)buffer, sizeof(expected)));
}

void page_test(void){
  replacement_test();
  read_page_test();
  write_page_test();
}
