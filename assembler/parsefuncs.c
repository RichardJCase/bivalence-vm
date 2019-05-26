#include "strings.h"
#include "parsefuncs.h"
#include "operations.h"

extern FILE *infile, *outfile;
static size_t instruction_offset = 0;

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

inline bool is_register(argument *arg){
  switch(arg->type){
  case SR:
  case UR:
  case FR:
    return true;
  case LIT:
  case NONE:
  default:
    return false;
  }
}

inline bool is_literal(argument *arg){
  return arg->type == LIT;
}

#define INVALID_ARGUMENT fatal("Invalid argument for instruction.")

#define WRITE_INSTRUCTION fwrite(&instruction, 8, 1, outfile)

#define REG_INST(INST)				\
  u64 instruction;				\
  argument reg;					\
  next_arg(&line, &reg);			\
  if(!is_register(&reg))			\
    INVALID_ARGUMENT;				\
						\
  switch(reg.type){				\
  case SR:					\
    instruction = INST ## _SR;			\
    break;					\
  case UR:					\
    instruction = INST ## _UR;			\
    break;					\
  case FR:					\
    instruction = INST ## _FR;			\
    break;					\
  case LIT:					\
  case NONE:					\
  default:					\
    INVALID_ARGUMENT;				\
  }						\
						\
  instruction_offset = OP_BITS

//todo: REG_ARG2 for two registers

#define LIT_ARG(N)				\
  argument lit##N;				\
  next_arg(&line, &lit##N);			\
  if(!is_literal(&lit##N))			\
    INVALID_ARGUMENT;				\
						\
  instruction +=				\
    lit##N.value << instruction_offset;		\
  instruction_offset += VALUE_BITS


void poke_parse(const char *line){
  REG_INST(POKE);
  LIT_ARG(1);

  WRITE_INSTRUCTION;
}

void peek_parse(const char *line){unused(line);}
void add_parse(const char *line){unused(line);}
void sub_parse(const char *line){unused(line);}
void mul_parse(const char *line){unused(line);}
void div_parse(const char *line){unused(line);}
void and_parse(const char *line){unused(line);}
void or_parse(const char *line){unused(line);}
void xor_parse(const char *line){unused(line);}
void mov_parse(const char *line){unused(line);}
void call_parse(const char *line){unused(line);}
void jmp_parse(const char *line){unused(line);}
void ret_parse(const char *line){unused(line);}
void stop_parse(const char *line){unused(line);}
