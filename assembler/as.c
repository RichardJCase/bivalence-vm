#include "common.h"
#include "strings.h"
#include "parsefuncs.h"

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
bool next_token(const char **buffer, token out_buffer){
  size_t n = 0;

  memset(out_buffer, 0, MAX_TOKEN_SIZE);
  
  while(is_whitespace(*buffer))
    ++*buffer;

  const char *start = *buffer;
  
  while(**buffer && !is_whitespace(*buffer)){
    if(++n >= MAX_TOKEN_SIZE)
      return false;
    
    ++*buffer;
  }

  if(!n)
    return false;

  strncpy(out_buffer, start, n);

  return true;
}

static void process_instruction(const token token, const char *line){
  if(string_equal("poke", token))
    poke_parse(line);
  else if(string_equal("peek", token))
    peek_parse(line);
  else if(string_equal("add", token))
    add_parse(line);
  else if(string_equal("sub", token))
    sub_parse(line);
  else if(string_equal("mul", token))
    mul_parse(line);
  else if(string_equal("div", token))
    div_parse(line);
  else if(string_equal("and", token))
    and_parse(line);
  else if(string_equal("or", token))
    or_parse(line);
  else if(string_equal("xor", token))
    xor_parse(line);
  else if(string_equal("mov", token))
    mov_parse(line);
  else if(string_equal("call", token))
    call_parse(line);
  else if(string_equal("jmp", token))
    jmp_parse(line);
  else if(string_equal("ret", token))
    ret_parse(line);
  else if(string_equal("stop", token))
    stop_parse(line);
}

static void process(const char *line){
  token token;

  if(!next_token(&line, token))
    return;

  if(token[0] == ';')
    return;

  if(string_equal(token, "DATA")){
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
  
  process_instruction(token, line);
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
