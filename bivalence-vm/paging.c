#include <unistd.h>
#include "common.h"

typedef struct {
  bool written;
  size_t use_id, start;
  byte memory[PAGE_SIZE];
} page;

static size_t file_position = 0;
static size_t page_counter = 0;
static page page_table[NUM_PAGE] = {0};

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

static size_t next_available_page(void){
  size_t oldest = 0;
  size_t oldest_value = page_table[0].use_id;
  for(size_t i = 1; i < NUM_PAGE; i++){
    if(!page_table[i].use_id){
      oldest = i;
      break;
    }

    if(page_table[i].use_id < oldest_value){
      oldest = i;
      oldest_value = page_table[i].use_id;
    }
  }

  page_table[oldest].use_id = ++page_counter;
  if(!page_counter)
    reset_page_ids();

  resort_pages(oldest);
  
  return oldest;
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

static size_t get_page(size_t page_start, size_t *page_index){
  *page_index = page_search(page_start); 
  if(*page_index == NUM_PAGE)
    return read_page(page_start, page_index);

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
