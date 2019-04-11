#include <stdio.h>
#include <math.h>
#include <assert.h>

#include "execute_test.h"

FILE *program;
cpu cores[MAX_THREADS] = {0};

void run_prog(cpu *core, const char * const path){
  if(program)
    fclose(program);

  memset(core, 0, sizeof(cpu));
  
  program = fopen(path, "r+");
  assert(program);
  assert(read_page(0, NULL));
  load_page(core, 0);
  assert(execute(core));
}

void assign_test(cpu *core){
  run_prog(core, "assign.b");

  assert(core->br);
  assert(core->sr[0] == -1);
  assert(core->ur[0] == 1);
  assert(isinf(core->fr[0]));
}

void math_test(cpu *core){
  run_prog(core, "math.b");

  assert(core->sr[0] == -2);
  assert(core->ur[0] == 4);
  assert(isnan(core->fr[0]));
  assert(core->fr[1] == 1);
  assert(core->fr[2] == 2);
}

void mem_test(cpu *core){
  run_prog(core, "mem.b");
  assert(core->execution_stack[0] == 5);
}

void logic_test(cpu *core){
  run_prog(core, "logic.b");
  assert(core->br);
  assert(core->ur[0] == 2);
}

void library_test(cpu *core){
  run_prog(core, "library.b");
  assert(core->br);
}

void execute_test(void){
  cpu cpu = {0};
  
  assign_test(&cpu);
  math_test(&cpu);
  mem_test(&cpu);
  logic_test(&cpu);
  library_test(&cpu);
  fclose(program);
}
