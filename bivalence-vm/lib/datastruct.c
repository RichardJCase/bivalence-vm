#include "datastruct.h"

typedef struct {
  pointer *data;
  size_t width, length;
  bool (*next)(void*);
} set_t;

//rtodo: will need to test to see if this is valid
#define vector_index (vector_ptr + index * vector->width)

bool vector_create(size_t width, size_t length, pointer *vector_ptr){
  if(!mempool_new(sizeof(set_t), vector_ptr))
    return false;

  (*vector_ptr)->width = width;
  (*vector_ptr)->length = length;
  return true;
}

bool vector_get(pointer vector_ptr, size_t index, void *out_item){
  set_t *vector;
  return mempool_get(vector_ptr, vector)
    && index >= vector->length
    && mempool_get(vector_index, out_item);
}

bool vector_insert(pointer vector_ptr, size_t index, void *item){
  set_t *vector;
  if(!mempool_get(vector_ptr, vector))
    return false;

  if(index > vector->length)
    return false;
  
  if(index == vector->length){
    pointer size;
    if(!space_to_allocate(vector->length * vector->width, &size))
      return false;
		
    if(!mempool_reserve(new_size))
      return false;
  }

  mempool_put(vector_index, vector->width);
  return true;
}

bool vector_add(pointer vector_ptr, void *item){
  set_t *vector;
  return mempool_get(vector_ptr, vector)
    && mempool_insert(vector_ptr, vector->length, item);
}

bool vector_remove(pointer vector_ptr, size_t index){
  set_t *vector;
  return mempool_get(vector_ptr, vector)
    && index < vector->length
    && mempool_delete(vector_index);
}

bool vector_clear(pointer vector_ptr){
  set_t *vector;
  if(!mempool_get(vector_ptr, vector))
    return false;
	
  while(vector->length){
    if(!vector_remove(vector_ptr, vector->length - 1))
      return false;
  }

  return true;
}

bool vector_copy(pointer *dest_vector_ptr, pointer vector_ptr){
  set_t *src, *dest;
  if(!mempool_get(vector_ptr, src)
     || !mempool_get(dest_vector_ptr, dest))
    return false;

  dest->width = src->width;
  
  if(!vector_clear(dest_vector_ptr))
    return false;
	
  for(size_t i = 0; i < src->length; i++){
    void *data = alloca(src->width);
    if(!vector_get(vector_ptr, i, data)
       || !vector_insert(dest_vector_ptr, data))
      return false;
  }

  return true;
}

//rtodo
bool set_create(size_t width, bool (*next)(void*), pointer *out_set_ptr){
  out_set_ptr->next = next;
  return vector_create(width, 1, out_set_ptr);
}

bool set_get(pointer set_ptr, size_t index, void *out_item){
  set_t *set;
  if(!mempool_get(set_ptr, set))
    return false;
  
  if(index > set->length){
    if(!set->next)
      return false;

    //rtodo: calculate it (use set_add)
  }

  return vector_get(set_ptr, index, out_item);
}

bool set_add(pointer set_ptr, void *item){
  return false;
}

bool set_remove(pointer set_ptr, size_t index){
  if(set->next)
    return false;

  return false;
}

bool set_clear(pointer set_ptr){
  return false;
}
