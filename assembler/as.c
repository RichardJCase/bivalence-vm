#include "common.h"
#include "strings.h"

FILE *outfile;

static void usage(void){
  puts("Usage: bas [file]");
}

static void process_instruction(const char *line){
  while(*line == '\t' || *line == ' ')
    ++line;

  //todo: process each instruction and append byte
}

static void process(const char *line){
  char token[MAX_TOKEN_SIZE];

  if(!next_token(line, token))
    return;

  if(token[0] == ';')
    return;

  if(string_equal("DATA", token)){
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
