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

bzr pull 2>&1 | grep "No revisions to pull"
if [ ! $? -eq 0 ]; then
    read -e -p "## Build and install emacs (revision $(bzr revno))? [Y/n] " yn
    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	cd $EMACS_SRC_DIR && pwd

	if [ -d "trunk" ]; then mv -v trunk/ trunk.bkp/ && rm -rf trunk; fi

	cp -Rv $EMACS_BZR_DIR/trunk/ $EMACS_SRC_DIR/
	cd trunk && ./autogen.sh && ./configure && make && sudo make install
    fi
fi
