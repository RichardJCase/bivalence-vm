#pragma once

#include "int.h"

#define MAX_TOKEN_SIZE 256

#define string_equal(s1, s2) (0 == strcmp(s1, s2))
#define is_whitespace(s) (*s == '\t' || *s == ' ' || *s == '\n')

typedef char token[MAX_TOKEN_SIZE];

bool next_token(const char **buffer, char *token);
