#include <pthread.h> 
#include "mem.h"

byte *pool = NULL;

size_t mempool_realloc_space(size_t pool_size, size_t pool_max){
	if(pool_max && pool_size > pool_max)
		return 0;
	
	size_t new_size = pool_size * 2;
	return new_size < pool_max ? new_size : pool_max;
}

mempool_space_func space_to_allocate = mempool_realloc_space;

static size_t mempool_size = 0;
static size_t mempool_used = 0;
static size_t next_slot = 0;

static size_t mempool_max = 0;
static size_t mempool_free_threshold = 1 << 30;

typedef struct {
	void *data;
	size_t size;
} memory_chunk;

memory_chunk pointers[MAX_ALLOCATIONS] = {0};

void set_mempool_max(size_t bytes){
	mempool_max = bytes;
}

void set_mempool_free_threshold(size_t bytes){
  mempool_free_threshold = bytes;
}

void set_mempool_space_func(mempool_space_func func){
	space_to_allocate = func;
}

bool mempool_reserve(size_t bytes){
	mempool_size += bytes;
	pool = realloc(pool, mempool_size);
	return NULL != pool;
}

bool mempool_new(size_t bytes, pointer *out_pointer){
	if(mempool_used + bytes >= mempool_size){
		size_t new_size = space_to_allocate(mempool_size, mempool_max);
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
