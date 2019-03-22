#include "cpu.h"

bool init_cores(void){
  size_t bytes_read = 0;
  if(!read_page(0, &bytes_read))
    return false;

  size_t to_init = (bytes_read < PAGE_SIZE) ? MAX_THREADS : 1;
    for(size_t i = 0; i < to_init; i++){
      if(!load_page(&cores[i]))
	return false;
    }

  return true;
}

static bool math_op(cpu *core, size_t op){
  switch(core->execution_stack[op]){
  case ADD:
  case SUB:
  case MUL:
  case DIV:
  case OR:
  case XOR:
  case AND:
  case LSL:
  case LSR:
    //todo: implement
    return true;
  default:
    return false;
  }

  return true;
}

static bool logic_op(cpu *core, size_t op){
  switch(core->execution_stack[op]){
  case CALL:
  case JMP:
    return true;
  default:
    return false;
  }
}

static bool library_op(cpu *core, size_t op){
  switch(core->execution_stack[op]){
    //ehuss.com/shared/
  default:
    fatal("illegal instruction.");
  }
}

static bool execute_op(cpu *core, size_t op){
  if(math_op(core, op))
    return true;

  if(logic_op(core, op))
    return true;
  
  if(library_op(core, op))
    return true;
  
  return false;
}

bool execute(cpu *core){
  for(size_t i = 0; i < PAGE_SIZE; i++){
    if(execute_op(core, i))
      return true;
  }
  
  return false;
}
