#include "set.h"
#include <stdlib.h>
#include <stdio.h>

void sset_insert(u8 *set, size_t size, u8 item){
  size_t high = size;
  size_t low = 0;
  size_t pivot;
  
  if(!set[0]){
    set[0] = item;
    return;
  }
  
  while(low < high){
    pivot = (high + low) / 2;
    
    if(set[pivot] && set[pivot] < item){
      low = pivot + 1;
    }else if(!set[pivot] || set[pivot] > item){
      if(!pivot) break;
      high = pivot - 1;
    }else{
      return;
    }
  }

  pivot = low;
  high = size - 1;
  while(low <= high && high){
    set[high + 1] = set[high];
    --high;
  }

  set[pivot] = item;
}

bool sset_contains(const u8 *set, size_t size, u8 item){
  size_t high = size;
  size_t low = 0;

  while(low < high){
    size_t pivot = (high + low) / 2;

    if(set[pivot] && set[pivot] < item){
      low = pivot + 1;
    }else if(!set[pivot] || set[pivot] > item){
      high = pivot;
    }else{
      return true;
    }
  }

  return false;
}
