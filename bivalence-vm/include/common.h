#pragma once
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "log.h"
#include "config.h"
#include "cpu.h"
#include "paging.h"
#include "operations.h"

#define unused(x) (void)(x)

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)
