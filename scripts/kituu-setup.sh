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
lisp[mail-bug]="git clone https://xaccrocheur@github.com/xaccrocheur/mail-bug.git"
lisp[nxhtml]="bzr branch lp:nxhtml"
# lisp[marker-visit]="git clone git://github.com/emacsmirror/marker-visit.git"
# lisp[emacs-powerline]="git clone https://github.com/jonathanchu/emacs-powerline.git"

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
	if [[ -e ~/$i ]] ; then mv -v ~/$i ~/$i.orig ; fi
	ln -sv $kituudir/$i ~/
    fi
done

type -P drakconf &>/dev/null || { mandriva=false >&2; }

packages="zsh curl gcc autoconf automake texinfo libtool libncurses5-dev libgnutls-dev librsvg2-dev imagemagick libgtk2.0-dev libxpm-dev libjpeg62-dev libtiff-dev libgif-dev  emacs zile wget bzr git perl-doc sox bbdb htop xfce4 bc thunderbird gimp inkscape wl"

read -e -p "Install various binary packages ($packages)? [Y/n] " yn
if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
    if $mandriva ; then
	sudo urpmi --auto $packages task-xfce task-xfce-plugins
    else
	sudo apt-get install $packages
    fi
fi

# if [ ! -e $scriptdir/git-completion.bash ] ; then
#     echo -e $sep"Git completion ($scriptdir/git-completion.bash)"
#     cd $scriptdir && curl -L https://github.com/git/git/raw/master/contrib/completion/git-completion.bash > $scriptdir/git-completion.bash
# fi

# if [ ! -e $scriptdir/leecher/leecher.pl ] ; then
#     echo -e $sep"leecher.pl ($scriptdir/leecher.pl)"
#     cd $scriptdir && git clone https://xaccrocheur@github.com/xaccrocheur/leecher.git
#     ln -sv $scriptdir/leecher/leecher.pl $scriptdir/
# else
#     cd $scriptdir/leecher/ && git pull
# fi

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

echo -e $sep"Emacs trunk"

read -e -p "Download, build and install / update (trunk) emacs? [Y/n] " yn
if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
    build-emacs.sh
fi

echo -e $sep"...Done."
