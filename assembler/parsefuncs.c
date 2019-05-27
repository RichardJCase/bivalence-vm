#include "strings.h"
#include "parsefuncs.h"
#include "operations.h"

extern FILE *infile, *outfile;
static size_t instruction_offset = 0;

/* dummy values for non-existant instructions */
enum {
  POKE_ADDR,
  PEEK_ADDR,
  ADD_ADDR,
  SUB_ADDR,
  MUL_ADDR,
  DIV_ADDR,
  AND_FR,
  AND_ADDR,
  OR_FR,
  OR_ADDR,
  XOR_FR,
  XOR_ADDR,
  CALL_SR,
  CALL_FR,
  JMP_SR,
  JMP_FR,
  LOAD_SR,
  LOAD_FR,
  LOAD_ADDR,
  SYM_SR,
  SYM_FR,
  SYM_ADDR,
  CCALL_SR,
  CCALL_FR,
  CCALL_ADDR
};

typedef enum {
  NONE,
  SR,
  UR,
  FR,
  LIT
} parse_type;

typedef struct {
  parse_type type;
  u32 value;
} argument;

static void next_arg(const char **line, argument *arg){
  token token;
  next_token(line, token);
  char *number = token + 1;
  
  switch(token[0]){
  case 's':
    arg->type = SR;
    if(!string_to_u32(number, &arg->value))
      fatal("Unable to parse index.");
    break;
  case 'u':
    arg->type = UR;
    if(!string_to_u32(number, &arg->value))
      fatal("Unable to parse index.");
    break;
  case 'f':
    arg->type = FR;
    if(!string_to_u32(number, &arg->value))
      fatal("Unable to parse index.");
    break;
  case '#':
    arg->type = LIT;
    if(!string_to_u32(number, &arg->value))
      fatal("Unable to parse unsigned value.");
    break;
  default:
    arg->type = NONE;
    arg->value = 0;
  }
}

#define IS_REGISTER_TYPE(TYPE) (TYPE == SR || TYPE == UR || TYPE == FR)

inline bool is_register(argument *arg){
  return IS_REGISTER_TYPE(arg->type);
}

#define IS_LITERAL_TYPE(TYPE) (TYPE == LIT)

inline bool is_literal(argument *arg){
  return IS_LITERAL_TYPE(arg->type);
}

#define INVALID_ARGUMENT fatal("Invalid argument for instruction.")

#define WRITE_INSTRUCTION fwrite(&instruction, 8, 1, outfile)
#define WRITE_LITERAL_INSTRUCTION(INST) unused(line); u64 instruction = INST; WRITE_INSTRUCTION

#define ALLOW_SR 1<<0
#define ALLOW_UR 1<<1
#define ALLOW_FR 1<<2
#define ALLOW_LIT 1<<3

#define PARSE_INST(INST, ALLOW_MASK)		\
  u64 instruction;				\
  argument reg;					\
  next_arg(&line, &reg);			\
  if(!is_register(&reg))			\
    INVALID_ARGUMENT;				\
  						\
  switch(reg.type){				\
  case SR:					\
    if(!(ALLOW_MASK & ALLOW_UR))		\
      INVALID_ARGUMENT;				\
    instruction = INST ## _SR;			\
    break;					\
  case UR:					\
    if(!(ALLOW_MASK & ALLOW_UR))	      	\
      INVALID_ARGUMENT;				\
    instruction = INST ## _UR;			\
    break;					\
  case FR:					\
    if(!(ALLOW_MASK & ALLOW_FR))       		\
      INVALID_ARGUMENT;	       			\
    instruction = INST ## _FR; 			\
    break;			       		\
  case LIT:					\
    if(!(ALLOW_MASK & ALLOW_LIT))		\
      INVALID_ARGUMENT;				\
    instruction = INST ## _ADDR;		\
    break;					\
  case NONE:					\
  default:					\
    INVALID_ARGUMENT;				\
  }						\
						\
  instruction_offset = OP_BITS

#define ANY_REG_INST(INST) PARSE_INST(INST, (ALLOW_SR | ALLOW_UR | ALLOW_FR))
#define INT_REG_INST(INST) PARSE_INST(INST, (ALLOW_SR | ALLOW_UR))
#define PTR_INST(INST) PARSE_INST(INST, (ALLOW_UR | ALLOW_LIT))

parse_type agnostic_arg(const char **line, u64 *instruction){
  argument arg;
  next_arg(line, &arg);

  instruction += arg.value << instruction_offset;
  instruction_offset += is_register(&arg) ? REG_BITS : VALUE_BITS;
  return arg.type;
}

#define ANY_ARG agnostic_arg(&line, &instruction)
#define LIT_ARG if(!IS_LITERAL_TYPE(ANY_ARG)) INVALID_ARGUMENT
#define REG_ARG if(!IS_REGISTER_TYPE(ANY_ARG)) INVALID_ARGUMENT
#define UR_ARG if(ANY_ARG != UR) INVALID_ARGUMENT

void poke_parse(const char *line){
  ANY_REG_INST(POKE);
  LIT_ARG;

  WRITE_INSTRUCTION;
}

void peek_parse(const char *line){
  ANY_REG_INST(PEEK);
  LIT_ARG;

  WRITE_INSTRUCTION;
}

#define WRITE_MATH_INST(INST)			\
  ANY_REG_INST(INST);		      		\
  ANY_ARG;					\
  ANY_ARG;					\
  WRITE_INSTRUCTION

void add_parse(const char *line){
  WRITE_MATH_INST(ADD);
}

void sub_parse(const char *line){
  WRITE_MATH_INST(SUB);
}

void mul_parse(const char *line){
  WRITE_MATH_INST(MUL);
}

void div_parse(const char *line){
  WRITE_MATH_INST(DIV);
}

#define WRITE_BIT_INST(INST)			\
  INT_REG_INST(INST);				\
  REG_ARG;					\
  REG_ARG;					\
  WRITE_INSTRUCTION

void and_parse(const char *line){
  WRITE_BIT_INST(AND);
}

void or_parse(const char *line){
  WRITE_BIT_INST(OR);
}

void xor_parse(const char *line){
  WRITE_BIT_INST(XOR);
}

void mov_parse(const char *line){
  //todo: own special logic
  unused(line);
}

void call_parse(const char *line){
  PTR_INST(CALL);
  WRITE_INSTRUCTION;
}

void jmp_parse(const char *line){
  PTR_INST(JMP);
  WRITE_INSTRUCTION;
}

void ret_parse(const char *line){
  WRITE_LITERAL_INSTRUCTION(RET);
}

void stop_parse(const char *line){
  WRITE_LITERAL_INSTRUCTION(STOP);
}

#define WRITE_LIB_INST(INST)			\
  PARSE_INST(INST, ALLOW_UR);			\
  UR_ARG;					\
  UR_ARG;					\
  WRITE_INSTRUCTION				

void load_parse(const char *line){
  WRITE_LIB_INST(LOAD);
}

void sym_parse(const char *line){
  WRITE_LIB_INST(SYM);
}

void ccall_parse(const char *line){
  WRITE_LIB_INST(CCALL);
}
