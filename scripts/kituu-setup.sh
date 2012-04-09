#!/bin/bash

shopt -s dotglob
kituudir=~/.kituu
lispdir=~/.emacs.d/lisp
scriptsdir=~/scripts
sep="\n################# "

echo -e $sep"Kituu! #################"

if [ ! -d $kituudir ]
then
echo -e $sep"No existing $kituudir, so"
    cd && git clone git@github.com:xaccrocheur/kituu.git
else
echo -e $sep"Found $kituudir, so"
    cd $kituudir && git pull
fi

for i in * ; do
    if [[ "${i}" != ".git" && "${i}" != "README.org" && "${i}" != "." && "${i}" != ".." && ! -L ~/$i && "${i}" != "*.elc" ]] ; then
	if [[ -e ~/$i ]] ; then
	    mv -v ~/$i ~/$i.orig
	    ln -sv $kituudir/$i ~/
	else
	    ln -sv $kituudir/$i ~/
	fi
    else
	[[ "${i}" != ".git" ]] && [[ "${i}" != "README.org" ]] && echo -e " \033[1m$i\033[0m \t > \t is already setup"
    fi
done

echo -e $sep"Various binary packages"
if [ command -v drakconf >/dev/null 2>&1 ] ; then
    sudo urpmi curl wget perl-doc sox bbdb
fi

if [ ! -e $scriptsdir/git-completion.bash ] ; then
    echo -e $sep"Git completion ($scriptsdir/git-completion.bash)"
    curl -L https://github.com/git/git/raw/master/contrib/completion/git-completion.bash > $scriptsdir/git-completion.bash
fi

if [ ! -e $scriptsdir/leecher/leecher.pl ] ; then
    echo -e $sep"leecher.pl ($scriptsdir/leecher.pl)"
    cd $scriptsdir && git clone git@github.com:xaccrocheur/leecher.pl.git
    ln -sv $scriptsdir/leecher/leecher.pl $scriptsdir/
else
    cd $scriptsdir/leecher/ && git pull
fi

if [ ! -e $lispdir/nxhtml/ ] ; then
    echo -e $sep"nXhtml ($lispdir/nxhtml/)"
    cd $lispdir/nxhtml/ && wget http://ourcomments.org/Emacs/DL/elisp/nxhtml/zip/nxhtml-2.08-100425.zip && unzip nxhtml-2.08-100425.zip
fi

echo -e $sep"Tabbar ($lispdir/tabbar/)"
if [ ! -e $lispdir/tabbar/ ] ; then
    cd $lispdir && git clone https://github.com/dholm/tabbar.git
else
    cd $lispdir/tabbar/ && git pull
fi

echo -e $sep"...Done."
