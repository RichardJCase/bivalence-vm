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
  register_number n = (register_number)(op & (ADDR_MASK << OP_BITS));
  address addr = op & (ADDR_MASK << (OP_BITS + REG_BITS));
  
  switch(op & OP_BITS){
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

  return true;
}

#undef peek
#undef poke

static bool math_op(cpu *core, instruction op){
  register_number n1, n2, n3;
  n1 = (register_number)(op & (REG_MASK << OP_BITS));
  n2 = (register_number)(op & (REG_MASK << (OP_BITS + REG_BITS)));
  n3 = (register_number)(op & (REG_MASK << (OP_BITS + 2 * REG_BITS)));
  
  switch(op & OP_BITS){
  case ADD_SR:
    core->sr[n1] = (i8)(core->sr[n2] + core->sr[n3]);
    break;
  case ADD_UR:
    core->ur[n1] = (u8)(core->ur[n2] + core->ur[n3]);
    break;
  case ADD_FR:
    core->fr[n1] = core->fr[n2] + core->fr[n3];
    break;
  case SUB_SR:
    core->sr[n1] = (i8)(core->sr[n2] - core->sr[n3]);
    break;
  case SUB_UR:
    core->ur[n1] = (u8)(core->ur[n2] - core->ur[n3]);
    break;
  case SUB_FR:
    core->fr[n1] = core->fr[n2] - core->fr[n3];
    break;
  case MUL_SR:
    core->sr[n1] = (i8)(core->sr[n2] * core->sr[n3]);
    break;
  case MUL_UR:
    core->ur[n1] = (u8)(core->ur[n2] * core->ur[n3]);
    break;
  case MUL_FR:
    core->fr[n1] = core->fr[n2] * core->fr[n3];
    break;
  case DIV_SR:
    core->sr[n1] = (i8)(core->sr[n2] / core->sr[n3]);
    break;
  case DIV_UR:
    core->ur[n1] = (u8)(core->ur[n2] / core->ur[n3]);
    break;
  case DIV_FR:
    core->fr[n1] = core->fr[n2] / core->fr[n3];
    break;
  case AND_SR:
    core->sr[n1] = (i8)(core->sr[n2] & core->sr[n3]);
    break;
  case AND_UR:
    core->ur[n1] = (u8)(core->ur[n2] & core->ur[n3]);
    break;
  case OR_SR:
    core->sr[n1] = (i8)(core->sr[n2] | core->sr[n3]);
    break;
  case OR_UR:
    core->ur[n1] = (u8)(core->ur[n2] | core->ur[n3]);
    break;
  case XOR_SR:
    core->sr[n1] = (i8)(core->sr[n2] ^ core->sr[n3]);
    break;
  case XOR_UR:
    core->ur[n1] = (u8)(core->ur[n2] ^ core->ur[n3]);
    break;
  default:
    return false;
  }

  return true;
}

static bool assign_op(cpu *core, instruction op){
  register_number n1, n2;
  n1 = (register_number)(op & (REG_MASK << OP_BITS));
  n2 = (register_number)(op & (REG_MASK << (OP_BITS + REG_BITS)));
  literal_value value = (literal_value)(op & (VALUE_MASK << (OP_BITS + REG_BITS)));
  
  switch(op & OP_BITS){
  case MOV_SR_SR:
    core->sr[n1] = core->sr[n2];
    break;
  case MOV_SR_UR:
    core->sr[n1] = (i8)core->ur[n2];
    break;
  case MOV_SR_FR:
    core->sr[n1] = (i8)core->fr[n2];
    break;
  case MOV_UR_SR:
    core->ur[n1] = (u8)core->sr[n2];
    break;
  case MOV_UR_UR:
    core->ur[n1] = core->ur[n2];
    break;
  case MOV_UR_FR:
    core->ur[n1] = (u8)core->fr[n2];
    break;
  case MOV_FR_SR:
    core->fr[n1] = (float)core->sr[n2];
    break;
  case MOV_FR_UR:
    core->fr[n1] = (float)core->ur[n2];
    break;
  case MOV_FR_FR:
    core->fr[n1] = core->fr[n2];
    break;
  case MOV_SR_N:
    core->sr[n1] = (i8)value;
    break;
  case MOV_UR_N:
    core->ur[n1] = (u8)value;
    break;
  case MOV_FR_N:
    core->fr[n1] = (float)value;
    break;
  default:
    return false;
  }

  return true;
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

  if(assign_op(core, op))
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
