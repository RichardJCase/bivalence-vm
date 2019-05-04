#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   sudo ./install.sh
   exit 0
fi

. configure.sh

make -C bivalence-vm install
make -C compiler install
make -C assembler install
