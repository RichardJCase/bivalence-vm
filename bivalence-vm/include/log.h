#pragma once

#include "messages.h"

#define _MSG_START "\033[0;31m%s::%s: L%d: " 
#define _MSG_END "\n\033[0m", __FILE__, __func__, __LINE__

#define error_fmt(msg, ...) shutdown(); fprintf(stderr, _MSG_START msg _MSG_END, __VA_ARGS__)
#define error(msg) shutdown(); fprintf(stderr, _MSG_START msg _MSG_END)

#define fatal_fmt(msg, ...) error("Bivalence VM has encountered a fatal error: " msg " Exiting.", __VA_ARGS__); exit(failure)
#define fatal(msg) error("Bivalence VM has encountered a fatal error: " msg " Exiting."); exit(failure)
