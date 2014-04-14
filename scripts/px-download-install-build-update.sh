#!/bin/bash

SRC_DIR=~/src

declare -A PACKS
# PACKS[00-lv2]="svn checkout http://lv2plug.in/repo/trunk"
# PACKS[01-drobilla-lad]="svn co http://svn.drobilla.net/lad/trunk"
# PACKS[03-ntk]="git clone git://git.tuxfamily.org/gitroot/non/fltk.git"
# PACKS[01-add64]="git clone git://git.code.sf.net/p/add64/code"
PACKS[02-triceratops]="git clone git://git.code.sf.net/p/triceratops/code"
# PACKS[02-amsynth]="git clone https://code.google.com/p/amsynth"
PACKS[02-drumkv1]="svn co http://svn.code.sf.net/p/drumkv1/code/trunk"
PACKS[02-samplv1]="svn co http://svn.code.sf.net/p/samplv1/code/trunk"
PACKS[02-synthv1]="svn co http://svn.code.sf.net/p/synthv1/code/trunk"
PACKS[03-sorcer]="git clone https://github.com/harryhaaren/openAV-Sorcer.git"
PACKS[03-qtractor]="svn co http://svn.code.sf.net/p/qtractor/code/trunk"
# PACKS[04-ardour]="git clone git://git.ardour.org/ardour/ardour.git"
PACKS[01-phasex]="git clone https://github.com/williamweston/phasex.git"
PACKS[00-calf]="git clone https://github.com/falkTX/calf.git"

BIN_BUILD="autoconf libqt4-dev dssi-dev librubberband-dev libboost-dev libglibmm-2.4-dev libsndfile-dev liblo-dev libxml2-dev uuid-dev libcppunit-dev libfftw3-dev libaubio-dev liblrdf-dev libsamplerate-dev libgnomecanvas2-dev libgnomecanvasmm-2.6-dev libcwiid-dev libgtkmm-2.4-dev libalsa-ocaml-dev libjack-jackd2-dev lv2-dev liblilv-dev libsuil-dev libsratom-dev liblash-compat-dev lv2-c++-tools libpaq-dev"

BIN_PLUGINS="invada-studio-plugins-lv2 invada-studio-plugins-ladspa so-synth-lv2 swh-lv2 mda-lv2 wsynth-dssi xsynth-dssi zynaddsubfx-dssi calf-plugins abgate aeolus amb-plugins autotalent caps cmt eq10q foo-yc20 hexter ir.lv2 lv2fil lv2vocoder mcp-plugins mda-lv2 swh-lv2 tap-plugins vocproc wah-plugins xsynth-dssi zita-at1 fluid-soundfont-gm amsynth whysynth"

BIN_PROD="linux-lowlatency qmidinet qjackctl vmpk"

BIN_PPA="ppa:rafalcieslak256/harmonyseq"

# if [[ ! $(grep ^ /etc/apt/sources.list /etc/apt/sources.list.d/* | cut -d: -f2,3 | sed '/^\#/d' | sed '/^$/d' | grep cassou) ]] ; then sudo add-apt-repository ppa:cassou/emacs && sudo apt-get update ; else echo -e "Emacs 24 repo \t\tOK" ; fi

echo "### $(basename $0) : ${#PACKS[@]} top-level repositories
## Use -f to ignore VC state & force build"

DEBIAN=$(type -P apt-get)
[[ $1 == "-f" ]] && FORCE_BUILD=true || FORCE_BUILD=false
[[ $1 == "-y" ]] && ALWAYS_YES=true || ALWAYS_YES=false

[[ -d $SRC_DIR ]] && cd $SRC_DIR || mkdir -v $SRC_DIR && cd $SRC_DIR

readarray -t PACKS_SORTED < <(printf '%s\n' "${!PACKS[@]}" | sort)

[[ $DEBIAN ]] && read -p "
## Install / Update build deps? ($BIN_BUILD) [Y/n] " YN || YN="no"
[[ $YN == "y" || $YN == "Y" || $YN == "" ]] && sudo apt-get install $BIN_BUILD
[[ $DEBIAN ]] && read -e -p "
## Install / Update prod apps? ($BIN_PROD) [Y/n] " YN || YN="no"
[[ $YN == "y" || $YN == "Y" || $YN == "" ]] && sudo apt-get install $BIN_PROD
[[ $DEBIAN ]] && read -e -p "
## Install / Update plugins? ($BIN_PLUGINS) [Y/n] " YN || YN="no"
[[ $YN == "y" || $YN == "Y" || $YN == "" ]] && sudo apt-get install $BIN_PLUGINS

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
        if [[ -f Makefile.svn ]] ; then
            [[ -f autogen.sh ]] && ./autogen.sh ||  make -f Makefile.svn
        elif [[ -f Makefile.am ]] ; then
            aclocal && autoconf && automake && autoheader
        fi
    fi

    if [[ $PACKAGE = "phasex" ]] ; then
        BUILD_FLAGS="--enable-arch=native --enable-parts=2"
    else
        BUILD_FLAGS=""
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
            if [[ $YN == "y" || $YN == "Y" || $YN == "" || $ALWAYS_YES ]] ; then
                [[ -f ./waf ]] && build_waf || build_make
            fi
        fi
    fi
}

for PACKAGE in "${PACKS_SORTED[@]}" ; do
    VC_SYSTEM=${PACKS[$PACKAGE]:0:3}
    [[ $VC_SYSTEM = "svn" ]] && VC_UPDATE_CMD="update" || VC_UPDATE_CMD="pull"
    PACKAGE_CLONE_COMMAND="${PACKS[$PACKAGE]}"
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
