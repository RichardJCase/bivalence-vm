#!/bin/false

export PAGE_SIZE=1024
export NUM_PAGE=4
export OPLVL="-O3"
export NUM_GP_REG=4
export MAX_THREADS=$(grep -c ^processor /proc/cpuinfo)
export INSTALL_DIR="/usr/bin"
export LOG_FILE="stderr"

# Page Replacement Algorithm
# 1. LFU
# 2. LRU
# 3. Both
# 4. mmap()
export PRA=4
