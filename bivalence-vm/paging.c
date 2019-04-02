#include <time.h>
#include <unistd.h>
#include "common.h"

typedef struct {
  size_t use_id, start;
  byte memory[PAGE_SIZE];
} page;

static size_t file_position = 0;
static size_t page_counter = 0;
static page page_table[NUM_PAGE] = {0};

static void reset_page_ids(void){
  size_t offset = page[0].use_id - 1;
  for(size_t i = 0; i < NUM_PAGE; i++)
    page[i].use_id -= offset;
}

static void resort_pages(size_t inserted){
  size_t n = inserted;
  while(n < NUM_PAGE - 1){
    page temp = pages[n];
    if(temp.start > pages[n+1].start)
      break;

    pages[n] = pages[n+1];
    pages[n+1] = temp;
    
    ++n;
  }

  while(n > 1){
    page temp = pages[n];
    if(temp.start < pages[n-1].start)
      break;

    pages[n] = pages[n-1];
    pages[n-1] = temp;
    
    --n;
  }
}

static size_t next_available_page(void){
  size_t index = 0;
  if(free_page(&index))
    return index;

  size_t oldest = 0;
  time_t oldest_value = page[0].use_id;
  for(size_t i = 1; i < NUM_PAGE; i++){
    if(!page[i].use_id){
      index = i;
      break;
    }

    if(page[i].use_id < oldest_value){
      oldest = i;
      oldest_value = page[i].use_id;
    }
  }

  page[index].use_id = ++page_counter;
  if(!page_counter)
    reset_page_ids();

  resort_pages(index);
  
  return index;
}

bool read_page(size_t page_start){
  long pos = fseek(program, (long)page_start, SEEK_CUR);
  if(pos == -1) return false;
  file_position = (size_t)pos;

  page page = page_table[next_available_page()];
  size_t bytes_read = fread(page->memory, 1, PAGE_SIZE, program);
  memset(page->memory + bytes_read, 0, PAGE_SIZE - bytes_read);

  page->start = page_start;
  
  return true;
}

bool read_page_bytes(size_t page_start, byte *bytes, size_t size){
  //todo: binary search page starts
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
