#pragma once

#include "mem.h"

typedef pointer Vector;
bool vector_create(size_t width, size_t length, Vector *out_vector_ptr);
bool vector_get(Vector vector_ptr, size_t index, void *out_item);
bool vector_add(Vector vector_ptr, void *item);
bool vector_insert(Vector vector_ptr, void *item);
bool vector_remove(Vector vector_ptr, size_t index);
bool vector_clear(Vector vector_ptr);
bool vector_copy(Vector dest_vector_ptr, Vector vector_ptr);

typedef pointer Set;
bool set_create(size_t width, bool (*add)(Set, void*), bool (*next)(void*, void*), Set *out_set_ptr);
bool set_get(Set set_ptr, size_t index, void *out_item);
bool set_add(Set set_ptr, void *item);
bool set_remove(Set set_ptr, size_t index);
bool set_clear(Set set_ptr);

typedef pointer Hash;
bool hash_create(size_t buckets, size_t width, bool (*hash)(void*, size_t), bool (*lookup)(void*, void*), Hash *hash_ptr);
bool hash_get(Hash hash_ptr, Hash *key, void *out_value);
bool hash_put(Hash hash_ptr, Hash *key, void *value);
bool hash_clear(Hash hash_ptr);
