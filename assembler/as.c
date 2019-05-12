#include "common.h"
#include "strings.h"
#include "parsefuncs.h"

#define STRING_EQUAL(s1, s2) (0 == strcmp(s1, s2))

FILE *outfile;

char *opcode_table[] = {
  "poke",
  "peek",
  "add",
  "sub",
  "mul",
  "div",
  "and",
  "or",
  "xor",
  "mov",
  "call",
  "jmp",
  "ret",
  "stop"
};

void (*parse_functions[])(const char*) = {
  poke_parse,
  peek_parse,
  add_parse,
  sub_parse,
  mul_parse,
  div_parse,
  and_parse,
  or_parse,
  xor_parse,
  mov_parse,
  call_parse,
  jmp_parse,
  ret_parse,
  stop_parse
};

static void usage(void){
  puts("Usage: bas [file]");
  exit(1);
}

//todo: place in strings file once it is made in the compiler
bool next_token(const char *buffer, char *out_buffer){
  const char *start = buffer;
  size_t n = 0;
  
  while(*buffer != ' ' && *buffer != '\t')
    ++buffer;

  while(*buffer != ' ' && *buffer != '\t'){
    if(++n >= MAX_TOKEN_SIZE)
      return false;
    
    ++buffer;    
  }

  if(!n)
    return false;

  strncpy(out_buffer, start, n);

  return true;
}

static void process_instruction(const char *line){
  while(*line == '\t' || *line == ' ')
    ++line;

  char token[MAX_TOKEN_SIZE];
  next_token(line, token);

  if(STRING_EQUAL("poke", token))
    poke_parse(line);
  else if(STRING_EQUAL("peek", token))
    peek_parse(line);
  else if(STRING_EQUAL("add", token))
    add_parse(line);
  else if(STRING_EQUAL("sub", token))
    sub_parse(line);
  else if(STRING_EQUAL("mul", token))
    mul_parse(line);
  else if(STRING_EQUAL("div", token))
    div_parse(line);
  else if(STRING_EQUAL("and", token))
    and_parse(line);
  else if(STRING_EQUAL("or", token))
    or_parse(line);
  else if(STRING_EQUAL("xor", token))
    xor_parse(line);
  else if(STRING_EQUAL("mov", token))
    mov_parse(line);
  else if(STRING_EQUAL("call", token))
    call_parse(line);
  else if(STRING_EQUAL("jmp", token))
    jmp_parse(line);
  else if(STRING_EQUAL("ret", token))
    ret_parse(line);
  else if(STRING_EQUAL("stop", token))
    stop_parse(line);
}

static void process(const char *line){
  char token[MAX_TOKEN_SIZE];

  if(!next_token(line, token))
    return;

  if(token[0] == ';')
    return;

  if(STRING_EQUAL(token, "DATA")){
    int data;
    i32 ret = sscanf(line, "%d", &data);
    while(ret > 0){
      if(data > 255){
	puts("Number exceeds byte limit.");
	exit(failure);
      }

      char byte = (char)data;
      fwrite(&byte, 1, 1, outfile);
    }
  }
  
  process_instruction(line);
}

static bool assemble(FILE *file){
  size_t line_length;
  ssize_t read;
  char *line = NULL;

  read = getline(&line, &line_length, file);
  while(read != -1){
    process(line);
    read = getline(&line, &line_length, file);
  }

  free(line);
  return true;
}

int main(int argc, char **argv){
  if(argc != 2)
    usage();

  FILE *file = fopen(argv[1], "r");
  if(!file){
    printf("Unable to open: %s\n", argv[1]);
    return failure;
  }

  bool assembled = assemble(file);
  
  fclose(file);
  
  return assembled ? success : failure;
}
