#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   sudo ./install.sh
   exit 0
fi

cd vm
./configure.sh
make install
