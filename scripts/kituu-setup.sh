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
    cd $kituudir && git fetch && git reset --hard origin/master
fi

for i in * ; do
    if [[ "${i}" != ".git" &&  "${i}" != "README.org" && "${i}" != "." && "${i}" != ".." && ! -L ~/$i ]] ; then
	if [[ -e ~/$i ]] ; then
	    mv -v ~/$i ~/$i.orig
	    ln -sv $kituudir/$i ~/
	else
	    ln -sv $kituudir/$i ~/
	fi
    else
	[[ "${i}" != ".git" ]] && [[ "${i}" != "README.org" ]] && echo -e " \033[1m$i\033[0m \t > \t is already a symlink"
    fi
done

if [ ! -e $scriptsdir/git-completion.bash ]
  then
    echo -e $sep"Installing Git completion..."
    curl -L https://github.com/git/git/raw/master/contrib/completion/git-completion.bash > $scriptsdir/git-completion.bash
fi

if [ ! -e $lispdir/tabbar/ ]
  then
    echo -e $sep"Installing Tabbar in $lispdir/tabbar/"
    cd $lispdir &&  git clone https://github.com/dholm/tabbar.git
    rm -rf tabbar/.git/* && rm -rfv $lispdir/tabbar/.git/ && echo -e $sep"...Done."
fi

if [ ! -e $scriptsdir/offlineimap/ ]
  then
    echo -e $sep"Installing offlineimap in $scriptsdir/offlineimap/"
    cd $scriptsdir && git clone https://github.com/spaetz/offlineimap.git
    ln -sv offlineimap/offlineimap.el . && echo -e $sep"...Done."
fi

if [ ! -e $lispdir/offlineimap.el ]
  then
    echo -e $sep"Installing offlineimap.el in $lispdir/"
    cd $lispdir && git clone http://git.naquadah.org/git/offlineimap-el.git
    cp -v offlineimap-el/offlineimap.el . && rm -rf offlineimap-el/* && rm -rfv $lispdir/offlineimap-el/ && echo -e $sep"...Done."
fi


