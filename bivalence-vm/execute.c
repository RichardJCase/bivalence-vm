#include <dlfcn.h>
#include "common.h"

#define poke(addr, reg) {if(!write_page_bytes(addr, (byte*)&reg, sizeof(reg))) fatal(FAILED_POKE);}
#define peek(addr, reg) {if(!read_page_bytes(addr, (byte*)&reg, sizeof(reg))) fatal(FAILED_PEEK);}

bool init_cores(void){
  read_page(0, NULL);
  
  for(size_t i = 0; i < MAX_THREADS; i++)
    load_page(&cores[i], 0);

  return true;
}

static bool mem_op(cpu *core, instruction op){
  register_number n = (register_number)(op & (ADDR_MASK << OP_BITS));
  address addr = op & (ADDR_MASK << (OP_BITS + REG_BITS));
  
  switch(op & OP_BITS){
  case POKE_SR:
    poke(addr, core->sr[n]);
    return true;
  case POKE_UR:
    poke(addr, core->ur[n]);
    return true;
  case POKE_FR:
    poke(addr, core->fr[n]);
    return true;
  case PEEK_SR:
    peek(addr, core->sr[n]);
    return true;
  case PEEK_UR:
    peek(addr, core->ur[n]);
    return true;
  case PEEK_FR:
    peek(addr, core->fr[n]);
    return true;
  default:
    return false;
  }

  return true;
}

static bool math_op(cpu *core, instruction op){
  register_number n1, n2, n3;
  n1 = (register_number)(op & (REG_MASK << OP_BITS));
  n2 = (register_number)(op & (REG_MASK << (OP_BITS + REG_BITS)));
  n3 = (register_number)(op & (REG_MASK << (OP_BITS + 2 * REG_BITS)));
  
  switch(op & OP_BITS){
  case ADD_SR:
    core->sr[n1] = (i16)(core->sr[n2] + core->sr[n3]);
    break;
  case ADD_UR:
    core->ur[n1] = (u16)(core->ur[n2] + core->ur[n3]);
    break;
  case ADD_FR:
    core->fr[n1] = core->fr[n2] + core->fr[n3];
    break;
  case SUB_SR:
    core->sr[n1] = (i16)(core->sr[n2] - core->sr[n3]);
    break;
  case SUB_UR:
    core->ur[n1] = (u16)(core->ur[n2] - core->ur[n3]);
    break;
  case SUB_FR:
    core->fr[n1] = core->fr[n2] - core->fr[n3];
    break;
  case MUL_SR:
    core->sr[n1] = (i16)(core->sr[n2] * core->sr[n3]);
    break;
  case MUL_UR:
    core->ur[n1] = (u16)(core->ur[n2] * core->ur[n3]);
    break;
  case MUL_FR:
    core->fr[n1] = core->fr[n2] * core->fr[n3];
    break;
  case DIV_SR:
    core->sr[n1] = (i16)(core->sr[n2] / core->sr[n3]);
    break;
  case DIV_UR:
    core->ur[n1] = (u16)(core->ur[n2] / core->ur[n3]);
    break;
  case DIV_FR:
    core->fr[n1] = core->fr[n2] / core->fr[n3];
    break;
  case AND_SR:
    core->sr[n1] = (i16)(core->sr[n2] & core->sr[n3]);
    break;
  case AND_UR:
    core->ur[n1] = (u16)(core->ur[n2] & core->ur[n3]);
    break;
  case OR_SR:
    core->sr[n1] = (i16)(core->sr[n2] | core->sr[n3]);
    break;
  case OR_UR:
    core->ur[n1] = (u16)(core->ur[n2] | core->ur[n3]);
    break;
  case XOR_SR:
    core->sr[n1] = (i16)(core->sr[n2] ^ core->sr[n3]);
    break;
  case XOR_UR:
    core->ur[n1] = (u16)(core->ur[n2] ^ core->ur[n3]);
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
    core->sr[n1] = (i16)core->ur[n2];
    break;
  case MOV_SR_FR:
    core->sr[n1] = (i16)core->fr[n2];
    break;
  case MOV_UR_SR:
    core->ur[n1] = (u16)core->sr[n2];
    break;
  case MOV_UR_UR:
    core->ur[n1] = core->ur[n2];
    break;
  case MOV_UR_FR:
    core->ur[n1] = (u16)core->fr[n2];
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
    core->sr[n1] = (i16)value;
    break;
  case MOV_UR_N:
    core->ur[n1] = (u16)value;
    break;
  case MOV_FR_N:
    core->fr[n1] = (float)value;
    break;
  default:
    return false;
  }

  return true;
}

static void call_common(cpu *core, u16 addr){
  poke(addr, core->bp);
  core->bp = core->sp + sizeof(size_t);
  core->sp = core->bp;
}

static bool logic_op(cpu *core, instruction op){
  register_number n = (register_number)(op & (ADDR_MASK << OP_BITS));
  address addr = op & (ADDR_MASK << OP_BITS);
  
  switch(op & OP_BITS){
  case CALL_ADDR:
    core->rp = core->ip;
    core->ip = addr;
    call_common(core, addr);
    break;
  case CALL_UR:
    core->rp = core->ip;
    core->ip = core->ur[n];
    call_common(core, addr);
    break;
  case JMP_ADDR:
    core->ip = addr;
    break;
  case JMP_UR:
    core->ip = core->ur[n];
    break;
  case RET:
    core->ip = core->rp;
    core->sp = core->bp;
    peek(addr, core->bp);
    break;
  default:
    return false;
  }

  return true;
}

static bool library_op(cpu *core, instruction op){
  register_number n1, n2, n3;
  n1 = (register_number)(op & (REG_MASK << OP_BITS));
  n2 = (register_number)(op & (REG_MASK << (OP_BITS + REG_BITS)));
  n3 = (register_number)(op & (REG_MASK << (OP_BITS + 2 * REG_BITS)));
  
  switch(op & OP_BITS){
  case LOAD:
    core->ur[n1] = (u16)dlopen((const char*)&core->ur[n2], (int)core->ur[n3]);
    return (bool)core->ur[n1];
  case SYM:
    core->ur[n1] = (u16)dlsym(&core->ur[n2], (const char*)&core->ur[n3]);
    return (bool)core->ur[n1];
  case CCALL:
    return ((bool (*)(void*))core->ur[n1])(&core->ur[n2]);
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
  if(core->ip >= PAGE_SIZE){
    if(!next_page(core))
      fatal("Unable to load page.");

    core->ip = 0;
  }

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
