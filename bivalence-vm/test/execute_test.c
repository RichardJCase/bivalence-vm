#include <stdio.h>
#include <assert.h>

#include "execute_test.h"

FILE *program;
cpu cores[MAX_THREADS] = {0};

void load_prog(cpu *cpu, const char * const path){
  program = fopen(path, "r+");
  assert(program);
  assert(read_page(0, NULL));
  load_page(cpu, 0);
}

void assign_test(cpu *cpu){
  load_prog(cpu, "assign.b");
}

void math_test(cpu *cpu){
  load_prog(cpu, "math.b");
}

void mem_test(cpu *cpu){
  load_prog(cpu, "mem.b");
}

void logic_test(cpu *cpu){
  load_prog(cpu, "logic.b");
}

void library_test(cpu *cpu){
  load_prog(cpu, "library.b");
}

void execute_test(void){
  cpu cpu = {0};
  
  assign_test(&cpu);
  math_test(&cpu);
  mem_test(&cpu);
  logic_test(&cpu);
  library_test(&cpu);
}
