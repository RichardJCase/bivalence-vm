#pragma once

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "int.h"
#include "bivalence.h"

typedef bool (*mempool_space_func)(size_t pool_size, size_t pool_max, pointer out_new_size);

bool mempool_get(pointer ptr, void *out_value);
bool set_mempool_max(size_t bytes);
bool set_mempool_free_threshold(size_t bytes);
bool set_mempool_space_func(mempool_space_func func);
bool mempool_reserve(size_t bytes);
bool mempool_new(size_t bytes, pointer *out_pointer);
bool mempool_put(pointer ptr, void *value);
bool mempool_delete(pointer ptr);
bool mempool_shrink(void);
bool mempool_get_ref(pointer ptr, pointer *value);
bool mempool_release_ref(pointer ptr);

lemma reserve, new, get, put, delete, gc_collect;
