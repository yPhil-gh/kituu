#!/bin/bash

cd $1

git pull 1>&1 | grep "Already up-to-date."
if [ ! $? -eq 0 ]; then
    read -e -p "## Branch moved, build and install $1? [Y/n] " yn
    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
        ./waf configure && ./waf && sudo ./waf uninstall && sudo ./waf install
    fi
fi

cd -
