#!/bin/bash

# Vars
shopt -s dotglob
repodir=~/.kituu
lispdir=~/.emacs.d/lisp
scriptdir=~/scripts
sep="\n################# "
rw=false
type -P apt-get &>/dev/null || { debian=true >&2; }
if [[ $1 = "-rw" ]]; then rw=true; fi
if ($rw); then vc_prefix="git@github.com:" && message="RW mode ON" && git config --global user.name "xaccrocheur" && git config --global user.email xaccrocheur@gmail.com ; else vc_prefix="https://github.com/" && message="RW mode OFF"; fi

if [[ ! $HOSTNAME == "N900" ]] ; then
    fancy_args="-v"
else
    fancy_args=""
fi

# Packages
declare -A pack
pack[dev_tools]="build-essential texinfo libtool"
pack[base_utils]="unison baobab gparted"
pack[view&players]="sox smplayer"
pack[image_tools]="gimp inkscape blender ffmpeg"
pack[music_prod]="qtractor invada-studio-plugins-lv2 ir.lv2 lv2fil mda-lv2 lv2vocoder so-synth-lv2 swh-lv2 vmpk qmidinet calf-plugins nekobee"
pack[games]="extremetuxracer supertuxkart stuntrally xonotic"
pack[emacs24_stable]="emacs24 emacs24-el emacs24-common-non-dfsg"
pack[emacs24_snapshot]="snapshot-el emacs-snapshot-gtk emacs-snapshot"

# Mozilla addons
mozurl="https://addons.mozilla.org/firefox/downloads/latest"
declare -A moz
moz[Uppity]="$mozurl/869/addon-869-latest.xpi"
moz[back_is_close]="$mozurl/939/addon-939-latest.xpi"
moz[Firebug]="$mozurl/1843/addon-1843-latest.xpi"
moz[GreaseMonkey]="$mozurl/748/addon-748-latest.xpi"
moz[GreaseMonkey_px_fix]="https://raw.github.com/xaccrocheur/kituu/master/scripts/gm-sane_inputs.user.js"
moz[French_dictionary_(save-as_for_thunderbird)]="$mozurl/354872/addon-354872-latest.xpi"
moz[tabmix+]="$mozurl/1122/addon-1122-latest.xpi"
moz[youtubeDLoader]="https://addons.mozilla.org/firefox/downloads/file/180678/youtube_video_mp3_downloader-2.1-fx.xpi?src=userprofile"
moz[adblock+]="$mozurl/1865/addon-1865-latest.xpi"
moz[color_picker]="$mozurl/271/addon-271-latest.xpi"
moz[TabCloser]="$mozurl/9669/addon-9669-latest.xpi"
moz[GreaseMonkey_jira]="https://raw.github.com/xaccrocheur/kituu/master/scripts/gm-jira.user.js"
moz[RequestPolicy]="$mozurl/9727/addon-9727-latest.xpi"

# Lisp packages
declare -A lisp
lisp[tabbar]="git clone https://github.com/dholm/tabbar.git"
lisp[haml-mode]="git clone https://github.com/nex3/haml-mode.git"
lisp[undo-tree]="git clone http://www.dr-qubit.org/git/undo-tree.git"
lisp[mail-bug]="git clone ${vc_prefix}xaccrocheur/mail-bug.git"
lisp[nxhtml]="bzr branch lp:nxhtml"

echo -e $sep"Kituu! #################

$message

Welcome to Kituu, $(whoami). This script allows you to install and maintain various packages from misc places. And well, do what you want done on every machine you install, and are tired of doing over and over again (tiny pedestrian things like create a "tmp" dir in your home).
You will be asked for every package (or group of packages in the case of binaries) if you want to install it ; After that you can run $(basename $0) again (it's in your PATH now if you use the dotfiles, specifically the .*shrc) to update the packages. Sounds good? Let's go."

echo -e $sep"Dotfiles and scripts"
read -e -p "#### Install / update dotfiles (in $HOME) and scripts (in $scriptdir)? [Y/n] " yn
if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
    if [ ! -d $repodir ] ; then
	cd && git clone ${vc_prefix}xaccrocheur/kituu.git
    else
	cd $repodir && git pull
    fi

    for i in * ; do
	if [[  ! -h ~/$i && $i != *#* && $i != *~* && $i != *git* && $i != "README.org" && $i != "." && "${i}" != ".." ]] ; then
	    if [[ -e ~/$i ]] ; then echo "(move)" && mv $fancy_args ~/$i ~/$i.orig ; fi
	    echo "(symlink)" && ln -s $fancy_args $repodir/$i ~/
	fi
    done
fi

echo -e $sep"Various menial janitor tasks"
if [[ ! -d ~/tmp ]] ; then mkdir -v ~/tmp ; else echo -e "~/tmp \t\t\tOK" ; fi
if [[ ! -d /mnt/tmp ]] ; then sudo mkdir -v /mnt/tmp ; else echo -e "/mnt/tmp \t\tOK" ; fi
if [[ ! $SHELL == "/bin/zsh" ]] ; then echo "Setting SHELL to zsh" && chsh -s /bin/zsh ; else echo -e "zsh shell \t\tOK" ; fi
gsettings set com.canonical.Unity.Panel systray-whitelist "['all']" && echo -e "Unity tray icons \tOK"

if [[ ! $(grep ^ /etc/apt/sources.list /etc/apt/sources.list.d/* | cut -d: -f2,3 | sed '/^\#/d' | sed '/^$/d' | grep cassou) ]] ; then sudo add-apt-repository ppa:cassou/emacs && sudo apt-get update ; else echo -e "Emacs 24 repo \t\tOK" ; fi

# Packages
if $debian; then
    echo -e $sep"Binary packages"
    read -e -p "#### Install packages? [Y/n] " yn
    echo -e $sep"Mandatory packages (no questions asked)"
    sudo apt-get install aptitude zsh vim byobu apt-file curl wget htop bc locate openssh-server sshfs bzr git subversion cowsay fortune fortunes-off zenity vinagre x11vnc ccze nmap

    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	for group in "${!pack[@]}" ; do
	    read -e -p "
## Install $group? (${pack[$group]})
[Y/n] " yn
	    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
		sudo aptitude install ${pack[$group]}
	    fi
	done
    fi
fi

# echo -e $sep"leecher.pl (a script to auto-get .ext links from a given web page URL)"
echo -e $sep"Leecher!"
if [ ! -e $scriptdir/leecher/leecher.pl ] ; then
    read -e -p "## Install leeecher (https://github.com/xaccrocheur/leecher)?  ($scriptdir/leecher.pl) [Y/n] " yn
if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
    cd $scriptdir && git clone ${vc_prefix}xaccrocheur/leecher.git
    ln -sv $scriptdir/leecher/leecher.pl $scriptdir/
fi
else
cd $scriptdir/leecher/ && git pull
fi

if [ ! -d "$lispdir" ] ; then mkdir -p $lispdir/ ; fi

echo -e $sep"Various repositories"
read -e -p "#### (e)Lisp stuff? [Y/n] " yn
if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
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
fi

if (type -P firefox &>/dev/null); then
    page=~/tmp/kituu-addons.html
    echo -e $sep"Mozilla add-ons"
    for addon in "${!moz[@]}" ; do
	addons=$addons"    <li><a href='"${moz[$addon]}"'>$addon</a></li>\n"
	addon_names=$addon", "$addon_names
    done
    read -e -p "Install add-ons ($addon_names)?
[Y/n] " yn
    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
	echo -e "
<html>
<head>
<style>
  body {font-family: sans-serif;background:#ccc;}
  hr {margin-top: 1em;width:35%;}
  img#logo {float:right;margin:1em;}
  img#id {width:25px;border:1px solid black;vertical-align:middle}
</style>
<title>Kituu: Install Mozilla addons for $(whoami)</title>
<link rel='shortcut icon' type='image/x-icon' href='http://mozilla.org/favicon.ico'></head>
<body style='background:#ccc'>
<a href='http://opensimo.org/play/?a=Azer0,Counternatures' title='Music!'>
<img id='logo' src='http://people.mozilla.com/~faaborg/files/shiretoko/firefoxIcon/firefox-128-noshadow.png' /></a>
  <h1>Hi $(whoami), click to install/update extension</h1>
  <ul>" > $page
echo -e $addons >> $page
echo -e "</ul>
  <hr />
  <div style='margin-left: auto;margin-right: auto;width:75%;text-align:center;'><a href='https://github.com/xaccrocheur/kituu'><img id='id' src='http://a0.twimg.com/profile_images/998643823/xix_normal.jpg' /></a>&nbsp;&nbsp;Don't forget that you're a genius, $(whoami) ;)</div>
</body>
</html>" >> $page && firefox $page &
	# echo $addons
    fi
fi

if [ -e $scriptdir/build-emacs.sh ]; then
    echo -e $sep"Emacs trunk"
    read -e -p "## Download, build and install / update (trunk: ~500Mb initial DL) emacs? [Y/n] " yn
    if [[ $yn == "y" || $yn == "Y" || $yn == "" ]] ; then
        # sudo apt-get install build-dep emacs23
	build-emacs.sh
    fi
fi

echo -e $sep"...Done."

# NOTES
# packages in probation: apt-file zile gdm xfce4 xfce4-terminal xfce4-goodies xfce4-taskmanager libgtk2.0-dev
# pack[glamp]="apache2 mysql-server phpmyadmin"
# Thunderbird no longer opens xpis. (But it has a new add-ons panel that makes it much easier to search&install them).
# moz[GreaseMonkey_style_fix]="http://userscripts.org/scripts/source/36850.user.js"
# pack[xscreensaver]="xscreensaver-gl xscreensaver-gl-extra xscreensaver-data-extra xscreensaver-data"
# pack[xubuntu]="xubuntu"
# pack[dev_env]="perl-doc"
# pack[dev_libs]="libncurses5-dev libgnutls-dev librsvg2-dev libxpm-dev libjpeg62-dev libtiff-dev libgif-dev libqt4-dev libgtk-3-dev"
# pack[emacs_friends]="bbdb mailutils w3m-el"
# http://archive.canonical.com/ubuntu/pool/partner/a/adobe-flashplugin/adobe-flashplugin_11.2.202.236-0precise1_i386.deb
# https://addons.mozilla.org/en-US/firefox/addon/youtubedownloader/?src=userprofile
# sudo netstat -tlnp
