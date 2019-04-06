#include "set_test.h"
#include "page_test.h"
#include "execute_test.h"
#include <stdio.h>
#include <assert.h>

FILE *program;

static void datastructure_tests(void){
  set_test();
}

int main(int argc, char **argv){
  (void)argc;
  (void)argv;

  program = fopen("test.b", "r+");
  assert(program);
  
  datastructure_tests();
  page_test();
  execute_test();

  puts("All tests successful.");
  return 0;
}
