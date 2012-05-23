#!/bin/bash

shopt -s dotglob
kituudir=~/.kituu
lispdir=~/.emacs.d/lisp
scriptdir=~/scripts
sep="\n################# "

# My lisp packages
declare -A lisp
lisp[tabbar]="git clone git://github.com/dholm/tabbar.git"
lisp[tabbar-ruler]="git clone git://github.com/mlf176f2/tabbar-ruler.el.git"
lisp[undo-tree]="git clone http://www.dr-qubit.org/git/undo-tree.git"
lisp[mail-bug.el]="git clone https://github.com/xaccrocheur/mail-bug.el"
lisp[nxhtml]="bzr branch lp:nxhtml"

echo -e $sep"Kituu! #################"

if [ ! -d $kituudir ] ; then
    echo -e $sep"No existing $kituudir, so"
    cd && git clone git@github.com:xaccrocheur/kituu.git
else
    echo -e $sep"Found $kituudir, so"
    cd $kituudir && git pull
fi

for i in * ; do
    if [[  ! -h ~/$i && $i != *#* && $i != *~* && $i != *git* && $i != "README.org" && $i != "." && "${i}" != ".." ]] ; then
	if [[ -e ~/$i ]] ; then
	    mv -v ~/$i ~/$i.orig
	    ln -sv $kituudir/$i ~/
	fi
    fi
done

type -P drakconf &>/dev/null || { mandriva=false >&2; }

if $mandriva ; then
    echo -e $sep"Various binary packages"
    sudo urpmi curl wget bzr git perl-doc sox bbdb htop
fi

if [ ! -e $scriptdir/git-completion.bash ] ; then
    echo -e $sep"Git completion ($scriptdir/git-completion.bash)"
    curl -L https://github.com/git/git/raw/master/contrib/completion/git-completion.bash > $scriptdir/git-completion.bash
fi

if [ ! -e $scriptdir/leecher/leecher.pl ] ; then
    echo -e $sep"leecher.pl ($scriptdir/leecher.pl)"
    cd $scriptdir && git clone git@github.com:xaccrocheur/leecher.pl.git
    ln -sv $scriptdir/leecher/leecher.pl $scriptdir/
else
    cd $scriptdir/leecher/ && git pull
fi

if [ ! -d "$lispdir" ] ; then mkdir -p $lispdir ; fi

for project in "${!lisp[@]}" ; do
    vcsystem=${lisp[$project]:0:3}
    echo -e $sep"$project ($lispdir/$project/)"
    if [ ! -e $lispdir/$project/ ] ; then
	read -e -p "Install $project in ($lispdir/$project/)? [Y/n] " yn
	if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	    cd $lispdir && ${lisp[$project]}
	fi
    else
	cd $lispdir/$project/ && $vcsystem pull
    fi
done

echo -e $sep"...Done."
