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

#define poke(reg) {if(!write_page_bytes(addr, (byte*)&reg, sizeof(reg))) fatal(FAILED_POKE);}
#define peek(reg) {if(!read_page_bytes(addr, (byte*)&reg, sizeof(reg))) fatal(FAILED_PEEK);}

static bool mem_op(cpu *core, instruction op){
  register_number n = (register_number)((instruction)op & (ADDR_MASK << OP_BITS));
  address addr = op & (ADDR_MASK << (OP_BITS + REG_BITS));
  
  switch(core->execution_stack[op]){
  case POKE_SR:
    poke(core->sr[n]);
    break;
  case POKE_UR:
    poke(core->ur[n]);
    break;
  case POKE_FR:
    poke(core->fr[n]);
    break;
  case PEEK_SR:
    peek(core->sr[n]);
    break;
  case PEEK_UR:
    peek(core->ur[n]);
    break;
  case PEEK_FR:
    peek(core->fr[n]);
    break;
  default:
    return false;
  }
}

#undef peek
#undef poke

static bool math_op(cpu *core, instruction op){
  unused(core); unused(op);
  return false;
}

static bool logic_op(cpu *core, instruction op){
  unused(core); unused(op);
  
  return false;
}

static bool library_op(cpu *core, instruction op){
  unused(core); unused(op);
  switch(core->execution_stack[op]){
    //ehuss.com/shared/
  default:
    return false;
  }
}

static bool execute_op(cpu *core, instruction op){
  if(mem_op(core, op))
    return true;
  
  if(math_op(core, op))
    return true;

  if(logic_op(core, op))
    return true;
  
  if(library_op(core, op))
    return true;
  
  return false;
}

static uint64_t next_instruction(cpu *core){
  //todo: if greater than page size than request next page
  return core->execution_stack[core->ip++];
}

bool execute(cpu *core){
  instruction current_instruction = next_instruction(core);
  while(current_instruction != STOP){
    if(current_instruction == UNINIT)
      fatal("STOP not reached before uninitialized instruction.");
    
    if(!execute_op(core, current_instruction))
      fatal("Illegal instruction.");

    current_instruction = next_instruction(core);
  }

  return true;
}
