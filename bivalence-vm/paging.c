#include <unistd.h>
#include "common.h"

#define LFU 1
#define LRU 2
#define BOTH 3

typedef struct {
  bool written;
  size_t use_id, uses, start;
  byte memory[PAGE_SIZE];
} page;

static size_t file_position = 0;
static size_t page_counter = 0;
static page page_table[NUM_PAGE] = {0};

static void reset_page_uses(size_t page_index){
  size_t offset = page_table[page_index].uses - 1;
  for(size_t i = 0; i < NUM_PAGE; i++)
    page_table[i].uses -= offset;
}

static void reset_page_ids(void){
  size_t offset = page_table[0].use_id - 1;
  for(size_t i = 0; i < NUM_PAGE; i++)
    page_table[i].use_id -= offset;
}

static void resort_pages(size_t inserted){
  size_t n = inserted;
  while(n < NUM_PAGE - 1){
    page temp = page_table[n];
    if(temp.start > page_table[n+1].start)
      break;

    page_table[n] = page_table[n+1];
    page_table[n+1] = temp;
    
    ++n;
  }

  while(n > 1){
    page temp = page_table[n];
    if(temp.start < page_table[n-1].start)
      break;

    page_table[n] = page_table[n-1];
    page_table[n-1] = temp;
    
    --n;
  }
}

static void increment_usage(size_t page_index){
  if(!(page_table[page_index].uses + 1))
    reset_page_uses(page_index);
  
  ++page_table[page_index].uses;
}

static void increment_last_used(size_t page_index){
  if(!(page_counter + 1))
    reset_page_ids();
  
  page_table[page_index].use_id = ++page_counter;
}

static void lfu_available_page(size_t *lfu_pages){
  //size_t lfu_value = 0;
  unused(lfu_pages);
  //todo
  
  for(size_t i = 0; i < NUM_PAGE; i++){
    
  }
}

static size_t lru_available_page(size_t *pages, size_t size){
  size_t oldest_index = 0, oldest_value = 0;
  
  for(size_t i = 0; i < size; i++){
    size_t use_id = page_table[pages[i]].use_id;
    if(!use_id){
      oldest_index = i;
      break;
    }
    
    if(use_id < oldest_value){
      oldest_index = i;
      oldest_value = use_id;
    }
  }

  increment_last_used(oldest_index);
  
  return oldest_index;
}

static size_t next_available_page(void){
  size_t optimal;

#if PRA != LRU
  size_t lfu_pages[NUM_PAGE] = {0};
  lfu_available_page(lfu_pages);
#endif
  
#if PRA == LFU
  unused(lru_available_page);
  optimal = lfu_pages[0];
#elif PRA == LRU
  unused(lfu_available_page);
  size_t pages[NUM_PAGE];
  for(size_t i = 0; i < NUM_PAGE; i++)
    pages[i] = i;
  
  optimal = lru_available_page(&pages, NUM_PAGE);
#elif PRA == BOTH
  size_t num_lfu_pages;
  for(num_lfu_pages = 1; num_lfu_pages < NUM_PAGE; num_lfu_pages++){
    if(!lfu_pages[num_lfu_pages])
      break;
  }
  
  optimal = lru_available_page(lfu_pages, num_lfu_pages);
#else
  #pragma GCC error "Invalid configuration of PRA parameter."
#endif

  resort_pages(optimal);
  return optimal;
}

bool read_page(size_t page_start, size_t *page_index){
  size_t tmp;
  if(!page_index)
    page_index = &tmp;

  *page_index = next_available_page();
  page page = page_table[*page_index];

  if(page.written)
    fwrite(page.memory, 1, PAGE_SIZE, program);
  
  long pos = fseek(program, (long)page_start, SEEK_CUR);
  if(pos == -1) return false;
  file_position = (size_t)pos;

  size_t bytes_read = fread(page.memory, 1, PAGE_SIZE, program);
  memset(page.memory + bytes_read, 0, PAGE_SIZE - bytes_read);

  page.start = page_start;
  
  return true;
}

static size_t page_search(size_t page_start){
  size_t high = NUM_PAGE;
  size_t low = 0;
  while(low < high){
    size_t pivot = (high + low) / 2;
    
    if(page_table[pivot].start < page_start){
      low = pivot + 1;
    }else if(page_table[pivot].start > page_start - PAGE_SIZE){
      high = pivot - 1;
    }else{
      return pivot;
    }
  }
  
  return NUM_PAGE;
}

static bool get_page(size_t page_start, size_t *page_index){
  *page_index = page_search(page_start); 
  if(*page_index == NUM_PAGE){
    if(!read_page(page_start, page_index))
      return false;
  }

  increment_usage(*page_index);
  return true;
}

bool read_page_bytes(size_t page_start, byte *bytes, size_t size){
  if(!size)
    return true;
  
  size_t page_index;
  if(!get_page(page_start, &page_index))
    return false;

  size_t to_read = (size > PAGE_SIZE) ? PAGE_SIZE : size;
  memcpy(bytes, page_table[page_index].memory, to_read);
  return read_page_bytes(page_start + to_read, bytes + to_read, size - to_read);
}

bool write_page_bytes(size_t page_start, byte *bytes, size_t size){
  if(!size)
    return true;
  
  size_t page_index;
  if(!get_page(page_start, &page_index))
    return false;

  size_t to_write = (size > PAGE_SIZE) ? PAGE_SIZE : size;
  memcpy(page_table[page_index].memory, bytes, to_write);

  page_table[page_index].written = true;
  
  return write_page_bytes(page_start + to_write, bytes + to_write, size - to_write);
}

void load_page(cpu *core, size_t page_number){
  core->execution_stack = page_table[page_number].memory;
}
