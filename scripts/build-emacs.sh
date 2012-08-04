#!/bin/bash

SRC_DIR=~/src

cd $HOME && pwd

if [ ! -d "$SRC_DIR" ]; then mkdir $SRC_DIR; fi

if [ ! -d "$SRC_DIR/emacs" ]; then
		cd $SRC_DIR && git clone git://git.sv.gnu.org/emacs.git && cd emacs
else
		cd $SRC_DIR/emacs
fi

pwd && git pull 1>&1 | grep "Already up-to-date."
if [ ! $? -eq 0 ]; then
    read -e -p "## Build and install emacs? [Y/n] " yn
    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
				make distclean && autoreconf -i -I m4 && ./configure && make && sudo make install
    fi
fi
