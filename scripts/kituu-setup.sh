#!/bin/bash

shopt -s dotglob
repodir=~/.kituu
lispdir=~/.emacs.d/lisp
scriptdir=~/scripts
sep="\n################# "
rw=false

type -P aptitude &>/dev/null || { debian=true >&2; }
if [[ $1 = "-rw" ]]; then rw=true; fi
if ($rw); then vc_prefix="git@github.com:" && message="RW mode ON";else vc_prefix="https://github.com/" && message="RW mode OFF"; fi

# My binary packages
declare -A pack
pack[base]="zsh curl zile wget bzr git sox htop bc unison thunderbird firefox locate filelight gparted"
pack[xfce]="gdm xfce4 xfce4-terminal xfce4-goodies xfce4-taskmanager"
pack[xscreensaver]="xscreensaver-gl xscreensaver-gl-extra xscreensaver-data-extra xscreensaver-data"
pack[dev_tools]="gcc autoconf automake texinfo libtool"
pack[dev_env]="perl-doc"
pack[dev_libs]="libncurses5-dev libgnutls-dev librsvg2-dev libgtk2.0-dev libxpm-dev libjpeg62-dev libtiff-dev libgif-dev imagemagick"
pack[emacs&friends]="emacs bbdb wl vim"
pack[image_tools]="gimp inkscape blender"
pack[multimedia]="clementine smplayer gstreamer0.10-plugins"
pack[music_prod]="qtractor invada-studio-plugins-lv2 ir.lv2 lv2fil mda-lv2 lv2vocoder so-synth-lv2 swh-lv2 vmpk"
pack[games]="extremetuxracer supertuxkart torcs nexuiz"

# My Mozilla addons
declare -A moz
moz[Uppity]="https://addons.mozilla.org/firefox/downloads/latest/869/addon-869-latest.xpi"
moz[back_is_close]="https://addons.mozilla.org/firefox/downloads/latest/939/addon-939-latest.xpi"
moz[Firebug]="https://addons.mozilla.org/firefox/downloads/latest/1843/addon-1843-latest.xpi"
moz[GreaseMonkey]="https://addons.mozilla.org/firefox/downloads/latest/748/addon-748-latest.xpi"
moz[GreaseMonkey_style_fix]="http://userscripts.org/scripts/source/36850.user.js"
moz[French_dictionary]="https://addons.mozilla.org/firefox/downloads/latest/354872/addon-354872-latest.xpi"

# My lisp packages
declare -A lisp
lisp[tabbar]="git clone https://github.com/dholm/tabbar.git"
lisp[tabbar-ruler]="git clone ${vc_prefix}xaccrocheur/tabbar-ruler.git"
lisp[undo-tree]="git clone http://www.dr-qubit.org/git/undo-tree.git"
lisp[mail-bug]="git clone ${vc_prefix}xaccrocheur/mail-bug.git"
lisp[nxhtml]="bzr branch lp:nxhtml"

echo -e $sep"Kituu! #################

$message

Welcome to Kituu. This script allows you to install and maintain various packages from misc places. And well, do what you want done on every machine you install, and are tired of doing over and over again (tiny pedestrian things like create a "tmp" dir in your home).
You will be asked for every package (or group of packages in the case of binaries) if you want to install it ; After that you can run $(basename $0) again (it's in your PATH now if you use the dotfiles, specifically the .*shrc) to update the packages. Sounds good? Let's go."

echo -e $sep"Dotfiles and scripts"
read -e -p "## Install dotfiles (in $HOME) and scripts (in $scriptdir)? [Y/n] " yn
if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
    if [ ! -d $repodir ] ; then
	cd && git clone ${vc_prefix}xaccrocheur/kituu.git
    else
	cd $repodir && git pull
    fi

    for i in * ; do
	if [[  ! -h ~/$i && $i != *#* && $i != *~* && $i != *git* && $i != "README.org" && $i != "." && "${i}" != ".." ]] ; then
	    if [[ -e ~/$i ]] ; then mv -v ~/$i ~/$i.orig ; fi
	    ln -sv $repodir/$i ~/
	fi
    done
fi

if [ ! -d ~/tmp ]; then
    mkdir -v ~/tmp
fi

if (! grep -q "deactivate" ~/.mplayer/config); then
    read -e -p "#### Setup xscreensaver? [Y/n] " yn
    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	echo 'heartbeat-cmd="xscreensaver-command -deactivate >&- 2>&- &"' | sudo tee -a /etc/apt/sources.list && sudo apt-get update
    fi
fi

if $debian; then
    echo -e $sep"Binary packages"
    read -e -p "#### Install packages? [Y/n] " yn
    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	for group in "${!pack[@]}" ; do
	    read -e -p "## Install $group? (${pack[$group]}) [Y/n] " yn
	    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
		sudo aptitude install ${pack[$group]}
	    fi
	done
    fi
fi

if (! grep -q "ubuntusatanic" /etc/apt/sources.list); then
    echo -e $sep"Theme (icons and stuff)"
    read -e -p "## Install dark theme? [Y/n] " yn
    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	wget -q http://ubuntusatanic.org/ubuntu-se-key.gpg -O- | sudo apt-key add -
	echo "deb http://ubuntusatanic.org/hell oneiric main" | sudo tee -a /etc/apt/sources.list && sudo apt-get update
	sudo apt-get install satanic-gnome-themes satanic-icon-themes
    fi
fi

echo -e $sep"leecher.pl (a script to auto-get .ext links from a given web page URL)"
if [ ! -e $scriptdir/leecher/leecher.pl ] ; then
    read -e -p "## Install leecher?  ($scriptdir/leecher.pl) [Y/n] " yn
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
	read -e -p "## Install $project in ($lispdir/$project/)? [Y/n] " yn
	if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	    cd $lispdir && ${lisp[$project]}
	fi
    else
	cd $lispdir/$project/ && $vcsystem pull
    fi
done

if [ -e $scriptdir/build-emacs.sh ]; then
    echo -e $sep"Emacs trunk"
    read -e -p "## Download, build and install / update (trunk) emacs? [Y/n] " yn
    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	build-emacs.sh
    fi
fi

if (type -P firefox &>/dev/null); then
    page=~/tmp/addons.html
    echo -e $sep"Mozilla add-ons"
    for addon in "${!moz[@]}" ; do
	addons=$addons"    <li><a href='"${moz[$addon]}"'>$addon</a></li>\n"
	addon_names=$addon", "$addon_names
    done
    read -e -p "Install add-ons ($addon_names)? [Y/n] " yn
    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	echo -e "
<html>
<head>
<style>
  body {
    font-family: sans-serif;
    background:#ccc;}
  hr {
    margin-top: 1em;
    width:35%;}
  img {
    float:right;
    margin-right:1em;}
</style>
  <!--link rel='stylesheet' media='screen,projection,tv' href='http://www.mozilla.org/media/css/firefox_new-min.css?build=193788b' /-->
</head>
<body style='background:#ccc'>
<a href='http://opensimo.org/play/?a=Azer0,Counternatures'><img src='http://a0.twimg.com/profile_images/998643823/xix_reasonably_small.jpg' /></a>
  <h1>Hi $(whoami), click to install/update extension</h1>
  <ul>" > $page
echo -e $addons >> $page
echo -e "</ul>
  <hr />
  <div style='margin-left: auto;margin-right: auto;width:75%;text-align:center;'><a href='https://github.com/xaccrocheur/kituu'>Kituu</a> is a <a href='https://plus.google.com/u/0/102175718864884791287'>xaccrocheur</a> production - don't forget that you're a genius too ;)</div>
</body>
</html>" >> $page && firefox $page &
	# echo $addons
    fi
fi

echo -e $sep"...Done."
