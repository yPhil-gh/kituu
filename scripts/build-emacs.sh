#!/bin/bash

SRC_DIR=~/src

cd $HOME

if [ ! -d "$SRC_DIR" ]; then mkdir $SRC_DIR; fi

if [ ! -d "$SRC_DIR/emacs" ]; then
    cd $SRC_DIR && pwd && git clone git://git.sv.gnu.org/emacs.git && cd emacs
else
    cd $SRC_DIR/emacs
fi

git pull 1>&1 | grep "Already up-to-date."
if [ ! $? -eq 0 ]; then
    read -e -p "## Branch moved, build and install emacs? [Y/n] " yn
    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	make distclean && autoreconf -i -I m4 && ./configure && make && sudo make install
	# make distclean && autoreconf -i -I m4 && ./configure --with-x-toolkit=gtk3 && make && sudo make install
    fi
fi
