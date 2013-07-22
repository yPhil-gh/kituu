#!/bin/bash

SRC_DIR=~/src

declare -A PACKS
PACKS[00-lv2]="svn checkout http://lv2plug.in/repo/trunk"
PACKS[01-drobilla-lad]="svn co http://svn.drobilla.net/lad/trunk"
PACKS[02-triceratops]="git clone git://git.code.sf.net/p/triceratops/code"
PACKS[02-amsynth]="git clone https://code.google.com/p/amsynth"
PACKS[02-drumkv1]="svn co http://svn.code.sf.net/p/drumkv1/code/trunk"
PACKS[02-samplv1]="svn co http://svn.code.sf.net/p/samplv1/code/trunk"
PACKS[02-synthv1]="svn co http://svn.code.sf.net/p/synthv1/code/trunk"
PACKS[03-qtractor]="svn co http://svn.code.sf.net/p/qtractor/code/trunk"
PACKS[04-ardour]="git clone git://git.ardour.org/ardour/ardour.git"

BINARIES="autoconf libqt4-dev libboost-dev libglibmm-2.4-dev libsndfile-dev liblo-dev libxml2-dev uuid-dev libcppunit-dev libfftw3-dev libaubio-dev liblrdf-dev libsamplerate-dev libgnomecanvas2-dev libgnomecanvasmm-2.6-dev libcwiid-dev libgtkmm-2.4-dev"

# END CONFIG

echo "### $(basename $0) : ${#PACKS[@]} top-level repositories
## Use -f to force build
"

INIT=true
DEBIAN=$(type -P apt-get)

[[ $1 == "-f" ]] && FORCE_BUILD=true || FORCE_BUILD=false
PACKS_INDEXES=( ${!PACKS[@]} )
PACKS_SORTED=( $(echo -e "${PACKS_INDEXES[@]/%/\n}" | sed -r -e 's/^ *//' -e '/^$/d' | sort) )
[[ -d $SRC_DIR ]] && cd $SRC_DIR || mkdir -v $SRC_DIR && cd $SRC_DIR

[[ $DEBIAN ]] && read -e -p "## Install / Update build deps? ($BINARIES) [Y/n] " YN || YN="no"
[[ $YN == "y" || $YN == "Y" || $YN == "" ]] && sudo apt-get install $BINARIES

function build_waf {
    if [[ $PACKAGE = "ardour" ]] ; then
	read -e -p "## Build ardour with Windows VST support? [Y/n] " YN
	if [[ $YN == "y" || $YN == "Y" || $YN == "" ]] ; then
            sudo apt-get install wine-dev
            BUILD_FLAGS="--windows-vst --program-name=ardour3-vst"
        else
            BUILD_FLAGS=""
        fi
    fi
    ./waf clean
    ./waf configure $BUILD_FLAGS && ./waf && sudo ./waf install
}

function build_make {
    if [[ $INIT ]] ; then
        [[ -f autogen.sh ]] && ./autogen.sh ||  make -f Makefile.svn
    fi
    make clean
    ./configure && make && sudo make install
}

function vc_check {
    [[ $VC_SYSTEM == "git" ]] && VC_LOG_COMMAND="git log -1" || VC_LOG_COMMAND="svn log -l 1"

    VC_PRE=$(${VC_LOG_COMMAND})
    $VC_SYSTEM $VC_UPDATE_CMD
    VC_POST=$(${VC_LOG_COMMAND})

    [[ "$VC_PRE" != "$VC_POST" ]] && return 0 || return 1
}

function update_package {
    if [[ $INIT = true || $FORCE_BUILD = true ]] ; then
        [[ -f ./waf ]] && build_waf || build_make
    else
        vc_check
        if [ $? -eq 0 ]; then
            read -e -p "## Branch moved, build and install $PACKAGE? [Y/n] " YN
            if [[ $YN == "y" || $YN == "Y" || $YN == "" || $INIT ]] ; then
                [[ -f ./waf ]] && build_waf || build_make
            fi
        fi
    fi
}

for PACKAGE in "${PACKS_SORTED[@]}" ; do
    VC_SYSTEM=${PACKS[$PACKAGE]:0:3}
    [[ $VC_SYSTEM = "svn" ]] && VC_UPDATE_CMD="update" || VC_UPDATE_CMD="pull"
    PACKAGE_CLONE_COMMAND="${PACKS[$PACKAGE]}"
    NAME_LENGTH=$(( ${#PACKAGE} -3 ))
    PACKAGE=${PACKAGE:3:$(( ${#PACKAGE} -3 ))}

    echo -e "\n## $PACKAGE"

    if [[ ! -d $SRC_DIR/$PACKAGE ]] ; then
        INIT=true
        # echo
        read -e -p "## Clone / Checkout $PACKAGE in ($SRC_DIR/$PACKAGE/)? [Y/n] " YN
        if [[ $YN == "y" || $YN == "Y" || $YN == "" ]] ; then
            cd $SRC_DIR && $PACKAGE_CLONE_COMMAND $PACKAGE && cd $PACKAGE && update_package
        fi
    else
        INIT=false
        cd $SRC_DIR/$PACKAGE && update_package
    fi
done
