#include "common.h"
#include "strings.h"
#include "parsefuncs.h"
#include "errno.h"

FILE *infile = NULL,
  *outfile = NULL,
  *log_file = NULL;

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

static void usage(void){
  fatal("Usage: bas [file]");
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
  else if(string_equal("load", token))
    load_parse(line);
  else if(string_equal("sym", token))
    sym_parse(line);
  else if(string_equal("ccall", token))
    ccall_parse(line);
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
      if(data > 255)
	fatal("Number exceeds byte limit.");

      char byte = (char)data;
      fwrite(&byte, 1, 1, outfile);
    }
  }
  
  process_instruction(token, line);
}

static bool assemble(void){
  size_t line_length;
  ssize_t read;
  char *line = NULL;

  read = getline(&line, &line_length, infile);
  while(read != EOF){
    process(line);
    read = getline(&line, &line_length, infile);
  }

  free(line);
  return true;
}

static void open_file(FILE **file, const char *name, const char *mode)
{
  *file = fopen(name, mode);
  if(!(*file))
    fatal_fmt("Unable to open: '%s'", name);
}

static void open_outfile(const char *in_file_name)
{
  size_t pos = (size_t)strchr(in_file_name, '.');
  if(!pos)
    fatal("Invalid file name.");
  
  pos -= (size_t)in_file_name;
    
  char *name = calloc(1, pos + 2);
  strncpy(name, in_file_name, pos);
  strcat(name, ".b");
  
  open_file(&outfile, name, "w");
  
  free(name);
}

void shutdown(void)
{
  if(outfile)
    fclose(outfile);

  if(infile)
    fclose(infile);
}

int main(int argc, char **argv){
  log_file = stdout;
  if(argc != 2)
    usage();

  open_file(&infile, argv[1], "r");
  open_outfile(argv[1]);
  
  bool assembled = assemble();
  
  shutdown();
  
  return assembled ? success : failure;
}
