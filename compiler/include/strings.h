#pragma once

#define MAX_TOKEN_SIZE 256

#include "int.h"

bool string_equal(const char *s1, const char *s2);

bool next_token(const char *buffer, char *token);
