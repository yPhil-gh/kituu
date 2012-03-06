#!/bin/bash

shopt -s dotglob
dir=~/.kituu
lispdir=~/.emacs.d/px-lisp/
sep="\n################# "

echo -e $sep"Kituu!"
# clone &| check
if [ ! -d $dir ]
then
    echo -e $sep"No existing $dir, cloning..."
    git clone git@github.com:xaccrocheur/kituu.git
else
    echo -e $sep"Found $dir, fetching..."
    cd $dir
    git fetch && git reset --hard origin/master
fi

# Install
for i in * ; do
    if [[ "${i}" != ".git" &&  "${i}" != "README.org" && "${i}" != "." && "${i}" != ".." && ! -L ~/$i ]] ; then
	if [[ -e ~/$i ]] ; then
	    mv ~/$i ~/$i.orig
	    echo -e "\033[1m~/$i\033[0m has been backuped to ~/$i.orig"
	    ln -s $dir/$i ~/
	    echo -e "~/$i is now a link to $dir/$i"
	else
	    ln -s $dir/$i ~/
	    echo -e "\033[1m~/$i\033[0m \t -> \t $dir/\033[1m$i\033[0m"
	fi
    else
	[[ "${i}" != ".git" ]] && [[ "${i}" != "README.org" ]] && echo -e " \033[1m$i\033[0m \t > \t is already a symlink"
    fi
done


# set up git completion
if [ ! -e ~/.scripts/git-completion.bash ]
  then
    echo -e $sep"Installing Git completion..."
    curl -L https://github.com/git/git/raw/master/contrib/completion/git-completion.bash > ~/.scripts/git-completion.bash
fi

# set up tabbar
cd $lispdir
if [ ! -e ./tabbar/ ]
  then
    echo -e $sep"Installing Tabbar..."
    git clone https://github.com/dholm/tabbar.git && echo -e $sep"...Done."
    rm -rf tabbar/.git/
fi
