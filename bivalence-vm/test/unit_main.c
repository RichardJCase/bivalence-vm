#include "set_test.h"
#include <stdio.h>

static void datastructure_tests(void){
  set_test();
}

int main(int argc, char **argv){
  (void)argc;
  (void)argv;
  
  datastructure_tests();

  puts("All tests successful.");
  return 0;
}
