#!/bin/bash

EMACS_BZR_DIR=~/src/emacs-tmp
EMACS_SRC_DIR=~/src/emacs-src

if [ ! -d "$EMACS_BZR_DIR" ]; then mkdir -pv $EMACS_BZR_DIR; fi
if [ ! -d "$EMACS_SRC_DIR" ]; then mkdir -pv $EMACS_SRC_DIR; fi

cd $EMACS_BZR_DIR && pwd

if [ ! -d "trunk" ]; then
    bzr branch bzr://bzr.savannah.gnu.org/emacs/trunk &&  cd trunk && pwd
else
    cd trunk && pwd
fi

bzr up 2>&1 | grep "Tree"
if [ ! $? -eq 0 ]; then
    cd $EMACS_SRC_DIR && pwd

    if [ -d "trunk" ]; then mv -v trunk trunk.bkp; fi

    cp -Rv $EMACS_BZR_DIR/trunk/ $EMACS_SRC_DIR/
    cd trunk && ./autogen.sh && ./configure && make && sudo make install
fi
