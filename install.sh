#!/bin/bash

if [[ $EUID -ne 0 ]]; then
   sudo ./install.sh
   exit 0
fi

. configure.sh

if [ ! -d $INSTALL_DIR ] 
then
		mkdir $INSTALL_DIR
fi

if [ ! -d $INSTALL_LIBDIR ]
then
		mkdir $INSTALL_LIBDIR
fi

make -C bivalence-vm install
make -C compiler install
make -C assembler install
