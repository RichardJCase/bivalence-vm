#include "datastruct.h"

typedef struct {
  pointer *data;
  size_t width, length;
  bool (*next)(void*);
  bool (*hash)(void*);
} set_t;

#define vector_index (vector->data + index * vector->width)

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
    && index < vector->length
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

  mempool_put(vector_index, item);
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

bool set_create(size_t width, bool (*hash)(Set, void*), bool (*next)(Set, void*), pointer *out_set_ptr){
  out_set_ptr->hash = hash ? hash : vector_add;
  out_set_ptr->next = next;
  return vector_create(width, 1, out_set_ptr);
}

bool set_get(pointer set_ptr, size_t index, void *out_item){
  set_t *set;
  if(!mempool_get(set_ptr, set))
    return false;
  
  if(index < set->length)
    return vector_get(set_ptr, index, out_item);
  
  if(!set->next)
    return false;

  void *last_item = alloca(set->width);
  if(!set_get(set_ptr, set->length - 1, last_item))
    return false;
    
  while(set->length - 1 != index){
    void *next_item = alloca(set->width);
    if(!set->next(last_item, next_item))
      return false;
      
    memcpy(last_item, next_item, set->width);
      
    if(!set_add(set_ptr, next_item))
      return false;
  }

  *out_item = last_item;
  return true;
}

bool set_add(pointer set_ptr, void *item){
  return set->hash(set_ptr, item);
}

bool set_remove(pointer set_ptr, size_t index){
  set_t *set;
  if(!mempool_get(set_ptr, set) || set_ptr->next)
    return false;

  return vector_remove(set_ptr, index);
}

bool set_clear(pointer set_ptr){
  set_t *set;
  if(!mempool_get(set_ptr, set) || set_ptr->next)
    return false;

  return vector_clear(set_ptr);
}

bool hash_create(size_t buckets, size_t width, bool (*hash)(void*, size_t), bool (*lookup)(void*, void*), Hash *hash_ptr){
  return false;
}

bool hash_get(Hash hash_ptr, Hash *key, void *out_value){
  return false;
}

bool hash_put(Hash hash_ptr, Hash *key, void *value){
  return false;
}

bool hash_clear(Hash hash_ptr){
  return false;
}
