#!/bin/bash

export PAGE_SIZE=1024
export OPLVL="-O3"
export NUM_GP_REG=4
export MAX_THREADS=$(grep -c ^processor /proc/cpuinfo)
export INSTALL_DIR="/usr/bin"
export STACK_SIZE=1024
