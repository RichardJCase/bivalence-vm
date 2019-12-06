#pragma once

#include "mem.h"

bool vector_create(size_t width, size_t length, pointer *out_vector_ptr);
bool vector_get(pointer vector_ptr, size_t index, void *out_item);
bool vector_add(pointer vector_ptr, void *item);
bool vector_insert(pointer vector_ptr, void *item);
bool vector_remove(pointer vector_ptr, size_t index);
bool vector_clear(pointer vector_ptr);
bool vector_copy(pointer dest_vector_ptr, pointer vector_ptr);

bool set_create(size_t width, bool (*next)(void*), pointer *out_set_ptr);
bool set_get(pointer set_ptr, size_t index, void *out_item);
bool set_add(pointer set_ptr, void *item);
bool set_remove(pointer set_ptr, size_t index);
bool set_clear(pointer set_ptr);

bool hash_create(size_t buckets, size_t width, bool (*hash_func)(void *, size_t), bool (*comp_func)(void *, void *, bool *), pointer *hash_ptr);
bool hash_get(pointer hash_ptr, pointer *key, void *out_value);
bool hash_put(pointer hash_ptr, pointer *key, void *value);
bool hash_clear(pointer hash_ptr);
