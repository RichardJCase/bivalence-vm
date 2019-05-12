#pragma once

#include "common.h"

void poke_parse(const char *line);
void peek_parse(const char *line);
void add_parse(const char *line);
void sub_parse(const char *line);
void mul_parse(const char *line);
void div_parse(const char *line);
void and_parse(const char *line);
void or_parse(const char *line);
void xor_parse(const char *line);
void mov_parse(const char *line);
void call_parse(const char *line);
void jmp_parse(const char *line);
void ret_parse(const char *line);
void stop_parse(const char *line);
