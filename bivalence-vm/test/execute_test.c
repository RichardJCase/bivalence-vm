#include "cpu.h"
#include "execute_test.h"

cpu cores[MAX_THREADS] = {0};

void load_prog(cpu *cpu, const char *const path){
  //todo
  (void)cpu;
  (void)path;
}

void assign_test(cpu *cpu){
  load_prog(cpu, "assign");
}

void math_test(cpu *cpu){
  load_prog(cpu, "math");
}

void mem_test(cpu *cpu){
  load_prog(cpu, "mem");
}

void logic_test(cpu *cpu){
  load_prog(cpu, "logic");
}

void library_test(cpu *cpu){
  load_prog(cpu, "library");
}

void execute_test(void){
  cpu cpu = {0};

  assign_test(&cpu);
  math_test(&cpu);
  mem_test(&cpu);
  logic_test(&cpu);
  library_test(&cpu);
}
