#include "args.h"

static args program_args = {0};

static bool usage(void){
  puts("RVM todo: copyright");
  puts("todo: arg header stuff");
  
  return false;
}

static bool apply_flags(void){
  for(size_t i = 0; i < NUM_FLAGS; i++){
    char flag = program_args.flags[i];
    switch(flag){
    case '\0':
      continue;
    case 'h':
    default:
      program_args.show_help = true;
      return true;
    }
  }

  return true;
}

static bool open_program(const char * const path){
  program = fopen(path, "r");
  if(program == NULL){
    printf("Unable to open %s\n", path);
    return false;
  }

  return true;
}

static bool check_args(void){
  if(!apply_flags())
    return usage();

  if(program_args.show_help)
    return usage();

  if(program_args.nonflag_index != program_args.nonflag_argc - 1)
    return usage();

  return open_program(program_args.nonflag_args[program_args.nonflag_index]);
}

bool process_args(u32 argc, char **argv){
  for(size_t i = 0; i < argc; i++){
    if(argv[0][0] == '-'){
      size_t len = strlen(argv[i]);
      for(size_t j = 1; j < len; j++)
	program_args.flags[j - 1] = argv[i][j];
    }else{
      program_args.nonflag_args[program_args.nonflag_argc++] = argv[i];
    }
  }

  return check_args();
}
