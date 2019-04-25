#include "set_test.h"
#include "page_test.h"
#include "execute_test.h"
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <stdio.h>
#include <assert.h>

static void datastructure_tests(void){
  set_test();
}

int main(int argc, char **argv){
  (void)argc;
  (void)argv;

  char *program_path = "test.b";
  
#if PRA == MMAP
  int tmpfile = open(program_path, O_RDWR, 0);
  if(!tmpfile){
    printf("Unable to open %s\n", program_path);
    return false;
  }

  struct stat st;
  stat(program_path, &st);
  long program_size = st.st_size;
  if(program_size <= 0){
    printf("Unable to open %s\n", program_path);
    return false;
  }
  
  program = mmap(NULL, (size_t)program_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_POPULATE, tmpfile, 0);
#else
  program = fopen(program_path, "r+");
#endif
  
  assert(program);
  
  datastructure_tests();
  page_test();
  execute_test();

  puts("All tests successful.");
  return 0;
}
