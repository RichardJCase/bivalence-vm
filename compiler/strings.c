#include "strings.h"
#include <stdio.h>

bool next_token(const char **buffer, token out_buffer){
  size_t n = 0;

  memset(out_buffer, 0, MAX_TOKEN_SIZE);
  
  while(is_whitespace(*buffer))
    ++*buffer;

  const char *start = *buffer;
  
  while(**buffer && !is_whitespace(*buffer)){
    if(++n >= MAX_TOKEN_SIZE)
      return false;
    
    ++*buffer;
  }

  if(!n)
    return false;

  strncpy(out_buffer, start, n);

  return true;
}

bool string_to_u32(const char *str, u32 *value){
  char *end = NULL;
  long result = strtol(str, &end, 10);
  
  if(str != end && 0 <= result && (u32)result <= UINT_FAST32_MAX){
    *value = (u32)result;
    return true;
  }
  
  return false;
}

bool string_to_i32(const char *str, i32 *value){
  char *end = NULL;
  long result = strtol(str, &end, 10);

  if(str != end && INT_FAST32_MIN <= result && result <= INT_FAST32_MAX){
    *value = result;
    return true;
  }
  
  return false;
}

bool string_to_float(const char *str, float *value){
  char *end = NULL;
  *value = strtof(str, &end);
  return str != end;
}
