#include <assert.h>
#include "int.h"
#include "set.h"
#include "set_test.h"

#define SET_SIZE 5

#include <stdio.h>

u8 expected[SET_SIZE] = {1, 2, 3, 5, 0};

void set_insert(u8 *set){
  sset_insert(set, SET_SIZE, 1);
  sset_insert(set, SET_SIZE, 5);
  sset_insert(set, SET_SIZE, 3);
  sset_insert(set, SET_SIZE, 2);

  for(u8 i = 0; i < SET_SIZE; i++)
    assert(set[i] == expected[i]);
}

void set_contains(const u8 *set){
  for(u8 i = 0; i < SET_SIZE - 1; i++)
    assert(sset_contains(set, SET_SIZE - 1, expected[i]));

  assert(!sset_contains(set, SET_SIZE, 4));
}

void set_test(void){
  u8 set[SET_SIZE] = {0};
  set_insert(set);
  set_contains(set);
}
