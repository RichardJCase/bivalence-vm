#!/bin/false

export PAGE_SIZE=1024*1024*8
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

#paths
export VM_INC="$PWD/bivalence-vm/include"

#compile options
export DEFINES="-DMAX_THREADS=$(echo $MAX_THREADS) -DPAGE_SIZE=$(echo $PAGE_SIZE) -DNUM_PAGE=$(echo $NUM_PAGE) -DNUM_GP_REG=$(echo $NUM_GP_REG) -DPRA=$(echo $PRA) -DLOG_FILE=$(echo $LOG_FILE)"
export WARNINGS="-Werror -Wfatal-errors -Wall -Wextra -Wpedantic -Wunused-macros -Wunsafe-loop-optimizations -Wunsuffixed-float-constants -Wtrampolines -Wswitch-enum -Wswitch-default -Wstrict-prototypes -Wstack-protector -Wsign-conversion -Wnull-dereference -Wmultichar -Winline -Wduplicated-cond -Wconversion -Walloc-zero -Walloca"

export CFLAGS="$(echo $OPLVL) -c $(echo $WARNINGS) $(echo $DEFINES) -fsplit-stack"
