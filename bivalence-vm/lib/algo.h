#pragma once

#include "datastruct.h"

bool sort(pointer *vector_ptr, lemma sort_func);
bool map(lemma map_func, pointer *vector_ptr, pointer *out_vector_ptr);

void compare(unsigned a, unsigned b){
  if(a > b)
    puts("a > b");
  else if(b > a)
    puts("b > a");
  else
    puts("they equal");
}


compare Nat a Nat b ->
  aGreater: greater a b -> puts 'a > b'.
  bGreater: greater b a -> puts 'b > a'.
  equal: and ~aGreater ~bGreater -> puts 'they equal'.
  all *.
