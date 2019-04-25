#pragma once

#include "common.h"

extern size_t program_size;

typedef enum {
  HELP,
  NUM_FLAGS
} flag_type;

typedef struct _args {
  bool show_help;

  size_t nonflag_argc, nonflag_index;
  const char *nonflag_args[NUM_FLAGS + 1];
  char flags[NUM_FLAGS];
} args;

bool process_args(u32 argc, char **argv);
