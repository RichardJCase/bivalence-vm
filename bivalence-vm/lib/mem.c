#include <pthread.h> 
#include "mem.h"

//where all of the data sections are stored contiguously
byte *pool = NULL;

static size_t mempool_size = 0;
static size_t mempool_used = 0;
static size_t next_slot = 0;

static size_t mempool_max = 0;

//when gc comes in to free memory; if a "page" can be freed
static size_t mempool_free_threshold = 1 << 30;

bool mempool_realloc_space(size_t pool_size, size_t pool_max, pointer out_new_size){
  if(pool_max && pool_size > pool_max)
    return true;
  
  size_t new_size = pool_size * 2;
  if(new_size < pool_max){
    mempool_put(out_new_size, &new_size);
    return true;
  }

  mempool_put(out_new_size, &pool_max);
  return false;
}

mempool_space_func space_to_allocate = mempool_realloc_space;

typedef struct {
  void *data;
  size_t size;
} memory_chunk;

memory_chunk pointers[MAX_ALLOCATIONS] = {0};

bool set_mempool_max(size_t bytes){
  mempool_max = bytes;
  return true;
}

bool set_mempool_free_threshold(size_t bytes){
  mempool_free_threshold = bytes;
  return true;
}

bool set_mempool_space_func(mempool_space_func func){
  space_to_allocate = func;
  return true;
}

bool mempool_reserve(size_t bytes){
  mempool_size += bytes;
  pool = realloc(pool, mempool_size);
  return NULL != pool;
}

bool mempool_new(size_t bytes, pointer *out_pointer){
  if(mempool_used + bytes >= mempool_size){

    //rtodo: manually create pointer
    pointer new_size;
    if(!space_to_allocate(mempool_size, mempool_max, new_size))
      return false;
    
    if(!new_size)
      return false;
		
    if(!mempool_reserve(new_size))
      return false;
  }

  memory_chunk chunk = {.data = pool + mempool_used, .size = bytes};

  pointers[next_slot] = chunk;
  *out_pointer = next_slot++;
  mempool_used += bytes;
  return true;
}

bool mempool_get(pointer ptr, void *out_value){
  if(!pointers[ptr].data)
    return false;

  memcpy(out_value, pointers[ptr].data, pointers[ptr].size);
  return true;
}

bool mempool_put(pointer ptr, void *value){
  if(!pointers[ptr].data)
    return false;

  memcpy(pointers[ptr].data, value, pointers[ptr].size);
  return true;
}

bool mempool_delete(pointer ptr){
  size_t old_used = mempool_used / mempool_free_threshold;

  mempool_used -= pointers[ptr].size;
  memset(&pointers[ptr], 0, sizeof(memory_chunk));
  memmove(&pointers[ptr].data, &pointers[ptr+1].data, (MAX_ALLOCATIONS) - (pointer)pointers[ptr+1].data);

  size_t new_used = mempool_used / mempool_free_threshold;

  if(new_used < old_used)
    pool = realloc(pool, mempool_free_threshold * new_used);

  return NULL != pool;
}

bool mempool_shrink(void){
  if(!mempool_used){
    free(pool);
    return true;
  }
	
  pool = realloc(pool, mempool_used);
  return NULL != pool;
}

prop reserve(pointer reserve_args){
  size_t bytes;
	
  if(!mempool_get(reserve_args, &bytes))
    return false;

  return mempool_reserve(bytes);
}

prop new(pointer new_args){
  size_t bytes;
	
  if(!mempool_get(new_args, &bytes))
    return false;

  return mempool_new(bytes, bytes + sizeof(bytes));
}

prop get(pointer get_args){
  pointer ptr;

  if(!mempool_get(get_args, &ptr))
    return false;

  return mempool_get(ptr, ptr + sizeof(ptr));
}

prop put(pointer put_args){
  pointer ptr;

  if(!mempool_get(put_args, &ptr))
    return false;

  return mempool_put(ptr, ptr + sizeof(ptr));
}

prop delete(pointer delete_args){
  pointer ptr;

  if(!mempool_get(put_args, &ptr))
    return false;

  return mempool_delete(ptr);
}

prop gc_collect(void){
  return mempool_shrink();
}
