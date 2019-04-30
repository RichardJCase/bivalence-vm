#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   sudo ./install.sh
   exit 0
fi

cd bivalence-vm
./configure.sh
make install
