#!/bin/bash

srcdir=~/src

declare -A pack
pack[00-lv2]="svn checkout http://lv2plug.in/repo/trunk"
pack[01-drobilla-lad]="svn co http://svn.drobilla.net/lad/trunk"
pack[02-triceratops]="git clone git://git.code.sf.net/p/triceratops/code"
pack[02-amsynth]="git clone https://code.google.com/p/amsynth"
pack[02-drumkv1]="svn co http://svn.code.sf.net/p/drumkv1/code/trunk"
pack[02-samplv1]="svn co http://svn.code.sf.net/p/samplv1/code/trunk"
pack[02-synthv1]="svn co http://svn.code.sf.net/p/synthv1/code/trunk"
pack[03-qtractor]="svn co http://svn.code.sf.net/p/qtractor/code/trunk"
pack[04-ardour]="git clone git://git.ardour.org/ardour/ardour.git"

# END CONFIG

pack_indexes=( ${!pack[@]} )
# IFS=$'\n'
pack_sorted=( $(echo -e "${pack_indexes[@]/%/\n}" | sed -r -e 's/^ *//' -e '/^$/d' | sort) )

[[ $1 == "-f" ]] && force=true || force=false
init=true

[[ -d $srcdir ]] && cd $srcdir || mkdir -v $srcdir && cd $srcdir

read -e -p "## Install deps? [Y/n] " yn
if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
    sudo apt-get install autoconf libboost-dev libglibmm-2.4-dev libsndfile-dev liblo-dev libxml2-dev uuid-dev libcppunit-dev libfftw3-dev libaubio-dev liblrdf-dev libsamplerate-dev libgnomecanvas2-dev libgnomecanvasmm-2.6-dev libcwiid-dev libgtkmm-2.4-dev
    # libsratom-dev libsuil-dev liblilv-0-0
fi

function build_waf {
    if [[ $1 = "ardour" ]] ; then
	read -e -p "## Build ardour with Windows VST support? [Y/n] " yn
	if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
            sudo apt-get install wine-dev
            build_flags="--windows-vst --program-name=ardour3-vst"
        else
            build_flags=""
        fi
    fi
    ./waf clean
    ./waf configure $build_flags && ./waf && sudo ./waf install
}

function build_make {
    if [[ $init ]] ; then
        [[ -f autogen.sh ]] && ./autogen.sh ||  make -f Makefile.svn
    fi
    make clean
    ./configure && make && sudo make install
}

function update_package {
    echo -e "\n## $1"
    if [[ $init = true || $force = true ]] ; then
        [[ -f ./waf ]] && build_waf $1 || build_make
    else
        if [[ $vcsystem == "git" ]] ; then
            git pull 1>&1 | grep "Already up-to-date."
        else
            svn up 1>&1 | grep "At revision"
        fi

        if [ ! $? -eq 0 ]; then
            read -e -p "## Branch moved, build and install $1? [Y/n] " yn
            if [[ $yn == "y" || $yn == "Y" || $yn == "" || $init ]] ; then
                [[ -f ./waf ]] && build_waf || build_make
            fi
        fi
    fi
}

for package in "${pack_sorted[@]}" ; do
    vcsystem=${pack[$package]:0:3}
    [[ $vcsystem = "svn" ]] && vcupdatecommand="update" || vcupdatecommand="pull"
    [[ $vcsystem = "svn" ]] && vcinitcommand="checkout" || vcinitcommand="clone"

    package_clone_command="${pack[$package]}"
    name_length=$(( ${#package} -3 ))
    package=${package:3:$name_length}

    if [[ ! -d $srcdir/$package ]] ; then
        init=true
        echo
        read -e -p "## $vcinitcommand $package in ($srcdir/$package/)? [Y/n] " yn
        if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
            cd $srcdir && $package_clone_command $package && cd $package
            update_package $package
        fi
    else
        init=false
        cd $srcdir/$package
        update_package $package
    fi
done
