#!/bin/bash

EMACS_BZR_DIR=~/tmp/emacs-src
EMACS_SRC_DIR=~/src/emacs-src

if [ ! -d "$EMACS_BZR_DIR" ]; then mkdir $EMACS_BZR_DIR; fi

if [ ! -d "$EMACS_SRC_DIR" ]; then mkdir $EMACS_SRC_DIR; fi

cd $EMACS_BZR_DIR;

if [ -d "trunk" ]; then
    cd trunk && bzr update;
else
    bzr branch bzr://bzr.savannah.gnu.org/emacs/trunk;
fi

cd $EMACS_SRC_DIR && rm -rf * && cp -R ~/tmp/emacs-src/trunk/ .;
cd trunk && ./autogen.sh && ./configure && make && sudo make install;
