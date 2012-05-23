#!/bin/bash

shopt -s dotglob
kituudir=~/.kituu
lispdir=~/.emacs.d/lisp
scriptsdir=~/scripts
sep="\n################# "

echo -e $sep"Kituu! #################"

# if [ ! -d $kituudir ] ; then
#     echo -e $sep"No existing $kituudir, so"
#     cd && git clone git@github.com:xaccrocheur/kituu.git
# else
#     echo -e $sep"Found $kituudir, so"
#     cd $kituudir && git pull
# fi

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

# if [ ! -e $scriptsdir/git-completion.bash ] ; then
#     echo -e $sep"Git completion ($scriptsdir/git-completion.bash)"
#     curl -L https://github.com/git/git/raw/master/contrib/completion/git-completion.bash > $scriptsdir/git-completion.bash
# fi

# if [ ! -e $scriptsdir/leecher/leecher.pl ] ; then
#     echo -e $sep"leecher.pl ($scriptsdir/leecher.pl)"
#     cd $scriptsdir && git clone git@github.com:xaccrocheur/leecher.pl.git
#     ln -sv $scriptsdir/leecher/leecher.pl $scriptsdir/
# else
#     cd $scriptsdir/leecher/ && git pull
# fi

# if [ ! -e $lispdir/nxhtml/ ] ; then
#     echo -e $sep"nXhtml ($lispdir/nxhtml/)"
#     cd $lispdir/ && wget http://ourcomments.org/Emacs/DL/elisp/nxhtml/zip/nxhtml-2.08-100425.zip && unzip nxhtml-2.08-100425.zip
# fi

# if [ ! -e $lispdir/tabbar/ ] ; then
#     echo -e $sep"Tabbar ($lispdir/tabbar/)"
#     cd $lispdir && git clone https://github.com/dholm/tabbar.git
# else
#     cd $lispdir/tabbar/ && git pull
# fi


# if [ ! -e $lispdir/tabbar-ruler/ ] ; then
#     echo -e $sep"Tabbar-ruler ($lispdir/tabbar-ruler/)"
#     cd $lispdir && git clone https://github.com/mlf176f2/tabbar-ruler.el.git
# else
#     cd $lispdir/tabbar-ruler/ && git pull
# fi

# if [ ! -e $lispdir/undo-tree/ ] ; then
#     echo -e $sep"Undo-tree ($lispdir/undo-tree/)"
#     cd $lispdir && git clone http://www.dr-qubit.org/git/undo-tree.git
# else
#     cd $lispdir/undo-tree/ && git pull
# fi

echo -e $sep"...Done."
