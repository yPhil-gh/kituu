#!/bin/bash

shopt -s dotglob
repodir=~/.kituu
lispdir=~/.emacs.d/lisp
scriptdir=~/scripts
sep="\n################# "
rw=false

type -P aptitude &>/dev/null || { debian=true >&2; }
if [[ $1 = "-rw" ]]; then rw=true; fi

if ($rw); then echo "rw!";fi
if ($rw); then vc_prefix="git@github.com:";else vc_prefix="https://github.com/"; fi

# My binary packages
declare -A pack
pack[base]="zsh curl zile wget bzr git sox htop bc unison thunderbird firefox locate filelight gparted"
pack[xfce]="gdm xfce4 xfce4-terminal xfce4-goodies xfce4-taskmanager"
pack[dev_tools]="gcc autoconf automake texinfo libtool"
pack[dev_env]="perl-doc"
pack[dev_libs]="libncurses5-dev libgnutls-dev librsvg2-dev libgtk2.0-dev libxpm-dev libjpeg62-dev libtiff-dev libgif-dev"
pack[emacs]="emacs bbdb wl"
pack[image_tools]="gimp inkscape imagemagick"
pack[multimedia]="clementine gstreamer0.10-plugins"

# My lisp packages
declare -A lisp
lisp[tabbar]="git clone https://github.com/dholm/tabbar.git"
lisp[tabbar-ruler]="git clone https://github.com/mlf176f2/tabbar-ruler.el.git"
lisp[undo-tree]="git clone http://www.dr-qubit.org/git/undo-tree.git"
lisp[mail-bug]="git clone ${vc_prefix}xaccrocheur/mail-bug.git"
lisp[nxhtml]="bzr branch lp:nxhtml"
# lisp[marker-visit]="git clone git://github.com/emacsmirror/marker-visit.git"
# lisp[emacs-powerline]="git clone https://github.com/jonathanchu/emacs-powerline.git"

echo -e $sep"Kituu! #################

Welcome to Kituu. This script allows you to install and maintain various packages from misc places.
You will be asked for every package if you want to install it ; After that you can run $(basename $0) again (it's in your PATH now) to update the packages. Sounds good? Let's go."

if $debian; then
    echo -e $sep"Binary packages"
    for group in "${!pack[@]}" ; do
	read -e -p "Install $group? (${pack[$group]}) [Y/n] " yn
	if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	    sudo aptitude install ${pack[$group]}
	fi
    done
fi

echo -e $sep"Dotfiles and scripts"
read -e -p "Install dotfiles (in $HOME) and scripts (in $scriptdir)? [Y/n] " yn
if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
    if [ ! -d $repodir ] ; then
	echo -e $sep"No existing $repodir, so"
	cd && git clone git@github.com:xaccrocheur/kituu.git
    else
	echo -e $sep"Found $repodir, so"
	cd $repodir && git pull
    fi

    for i in * ; do
	if [[  ! -h ~/$i && $i != *#* && $i != *~* && $i != *git* && $i != "README.org" && $i != "." && "${i}" != ".." ]] ; then
	    if [[ -e ~/$i ]] ; then mv -v ~/$i ~/$i.orig ; fi
	    ln -sv $repodir/$i ~/
	fi
    done
fi

if (! grep "ubuntusatanic" /etc/apt/sources.list &>/dev/null); then
    read -e -p "Install dark theme? [Y/n] " yn
    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	wget -q http://ubuntusatanic.org/ubuntu-se-key.gpg -O- | sudo apt-key add -
	echo "deb http://ubuntusatanic.org/hell oneiric main" | sudo tee -a /etc/apt/sources.list && sudo apt-get update
	sudo apt-get install xubuntu-satanic
    fi
fi

echo -e $sep"leecher.pl (a script to auto-get .ext links from a given web page URL)"
if [ ! -e $scriptdir/leecher/leecher.pl ] ; then
    read -e -p "Install leecher?  ($scriptdir/leecher.pl) [Y/n] " yn
    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	cd $scriptdir && git clone ${vc_prefix}xaccrocheur/leecher.git
	ln -sv $scriptdir/leecher/leecher.pl $scriptdir/
    else
	cd $scriptdir/leecher/ && git pull
    fi
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

echo -e $sep"Emacs trunk"

read -e -p "Download, build and install / update (trunk) emacs? [Y/n] " yn
if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
    build-emacs.sh
fi

if [ ! -d "~/ploup" ]; then
    mkdir -v ~/ploup
fi

echo -e $sep"...Done."
