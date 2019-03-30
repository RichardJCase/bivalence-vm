#include "execute.h"
#include "args.h"

cpu cores[MAX_THREADS] = {0};

static void shutdown(void){
  fclose(program);
  program = NULL;
}

int main(int argc, char **argv){
  if(!process_args((u32)argc - 1, argv + 1))
    return failure;

  init_cores();

  if(!execute(&main_core))
    return failure;

  shutdown();
  return success;
}
