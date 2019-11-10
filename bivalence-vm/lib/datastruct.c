#include "datastruct.h"

//rtodo: actually use data pointer

typedef struct {
	pointer *data;
	size_t width, length;
} vector_t;

#define vector_index (vector_ptr + index * vector->width)

//todo: set mempool space func

bool vector_create(size_t width, size_t length, pointer *vector_ptr){
	if(!mempool_new(sizeof(vector_t), vector_ptr))
		return false;

	(*vector_ptr)->width = width;
	(*vector_ptr)->length = length;
}

bool vector_get(pointer vector_ptr, size_t index, void *out_item){
	return mempool_get(vector_index, out_item);
}

bool vector_insert(pointer vector_ptr, size_t index, void *item){
	vector_t *vector;
	if(!mempool_get(*vector_ptr, vector))
		return false;

	if(index == vector->length){
		pointer size;
		if(!mempool_space_func(vector->length * vector->width, &size))
			return false;
		
		if(!mempool_reserve(new_size))
			return false;
	}

	if(index > vector->length){
		return false;
	}

	return true;
}

bool vector_add(pointer vector_ptr, void *item){
	vector_t *vector;
	if(!mempool_get(*vector_ptr, vector))
		return false;
	
	return mempool_insert(vector_ptr, vector->length, item);
}

bool vector_remove(pointer vector_ptr, size_t index){
	vector_t *vector;
	if(!mempool_get(vector_ptr, vector))
		return false;

	if(index > vector->length)
		return false;
	
	if(!mempool_delete(vector_index))
		return false;
	
	return true;
}

bool vector_clear(pointer vector_ptr){
	vector_t *vector;
	if(!mempool_get(vector_ptr, vector))
		return false;
	
	while(vector->length){
		if(!vector_remove(vector_ptr, vector->length - 1))
			return false;
	}

	return true;
}

bool vector_copy(pointer *dest_vector_ptr, pointer vector_ptr){

}
