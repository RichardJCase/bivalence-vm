#include "cpu.h"

bool load_page(cpu *core){
  memcpy(core->execution_stack, bytecode_page, PAGE_SIZE);
  return true; //todo: more logic later or make void
}
