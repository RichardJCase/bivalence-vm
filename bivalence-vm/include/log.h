#pragma once

#include <stdio.h>
#include "messages.h"

#define _MSG_START "\033[0;31m%s::%s: L%d: " 
#define _MSG_END "\n\033[0m", __FILE__, __func__, __LINE__
#define _FATAL_MSG "Encountered a fatal error: "

#define error_fmt(msg, ...) {fprintf(log_file, _MSG_START msg _MSG_END, __VA_ARGS__);}
#define error(msg) {fprintf(log_file, _MSG_START msg _MSG_END);}

#define fatal_fmt(msg, ...) {error_fmt(_FATAL_MSG msg " Exiting.", __VA_ARGS__); shutdown(); exit(failure);}
#define fatal(msg) {error(_FATAL_MSG msg " Exiting."); shutdown(); exit(failure);}

extern FILE *log_file;
