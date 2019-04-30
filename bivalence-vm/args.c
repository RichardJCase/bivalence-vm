#include "args.h"

static args program_args = {0};
size_t program_size = 0;
FILE *log_file = NULL;

static bool usage(void){
  puts(USAGE);
  
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
#if PRA == MMAP
  int tmpfile = open(path, O_RDWR, 0);
  if(!tmpfile){
    error_fmt(UNABLE_TO_OPEN, path);
    return false;
  }

  struct stat st;
  stat(path, &st);
  long st_size = st.st_size;
  if(st_size < 0)
    return false;

  program_size = (size_t)st_size;
  
  program = mmap(NULL, program_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_POPULATE, tmpfile, 0);
#else
  program = fopen(path, "r+");
#endif

  if(!program){
    error_fmt(UNABLE_TO_OPEN, path);
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

bool open_log_file(void){
#if LOG_FILE == stderr
  log_file = stderr;
  return true;
#else
  log_file = fopen(#LOG_FILE, "w");
  return log_file;
#endif
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

  if(!open_log_file())
    return false;
  
  return check_args();
}
