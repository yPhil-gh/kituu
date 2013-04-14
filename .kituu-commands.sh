# Aliases
# alias ls='ls -F --color=auto'
alias rm="rm -i"
alias cp="cp -i"

# if ! test "$HOSTNAME" = "N900" ; then
#     grep () { command grep --color $* }
# fi

# grep () { grep --color=always $* }


if [[ ! $HOSTNAME == "N900" ]] ; then
    # echo "kk"
    # alias grep="grep --color"
    alias grep="grep -RnIs --color"
fi

alias ll="ls -lha"
alias la="ls -A"

alias k="cd ~/.kituu/"
alias m="cd ~/.emacs.d/lisp/mail-bug/"
alias a="cd /var/www/adamweb/git.adamweb"

alias pss='ps aux | grep $(echo $1 | sed "s/^\(.\)/[\1]/g")'
alias mss="sudo cat /var/log/syslog | grep $1"
alias uss="urpmq -Y --summary"
alias rss="rpm -qa|grep -i"
alias rssi="rpm -qil"
alias MSG="sudo tail -f -n 40 /var/log/syslog | ccze"
alias MSGh="sudo tail -f -n 40 /var/log/httpd/error_log"
alias U="urpmi"
alias Commit="git commit -am"
alias Push="git push origin"
alias Syncmail="offlineimap.py -o -u blinkenlights; reset"
alias I="sudo apt-get install"
alias S="sudo apt-cache search"
# Commands

# alias ssh="tmux rename-window `echo $1 | sed 's/.*@//g'` ; command ssh $*"
# alias ssh='tmux rename-window $1 ; echo "plop"'

function ssh () {
    tmux rename-window `echo $1 | sed 's/.*@//g'`
    command ssh $1 ; echo "dead"
}

md () {
    mkdir -p $1
    cd $1
}

px-sshmount () {
    if [ ! grep -q "fuse.*$USER" /etc/group ] ; then sudo gpasswd -a $USER fuse && echo "added $USER to group fuse" ; fi
    if [ ! -n "$2" ] ; then fusermount -u $1 && echo "Unmounted $1" ; else sshfs -o idmap=user $1 $2 ; fi
}

px-vnc () {
    \ssh -f -L 5900:127.0.0.1:5900 $1 "x11vnc -scrollcopyrect -noxdamage -localhost -nopw -once -display :0" ; vinagre 127.0.0.1:5900
}

px-update-N900 () {
    rm .bashrc .kituu-commands.sh -f
    wget --no-check-certificate -nc https://github.com/xaccrocheur/kituu/raw/master/.kituu-commands.sh https://github.com/xaccrocheur/kituu/raw/master/.bashrc
    bash
}
px-sync-pr0n () {
    if [[ ! -n $2 ]] ; then echo "Usage : px-sync-pr0n [machine] [username]" && exit 1 ; fi
    if [[ $1 == "N900" ]] ; then my_LocalUSER="user" ; MyPath="$my_LocalUSER/MyDocs/tmp/" ; else my_LocalUSER=$2 ; fi
    # echo "plop"
    # echo "mounting $2@$1:/home/$my_LocalUSER/MyDocs/tmp/.pr0n/ to ~/tmp/$1/.pr0n/"
    px-sshmount $2@$1:/home/$my_LocalUSER/tmp/.pr0n/ ~/tmp/$1/.pr0n/ && echo "mounting $2@$1:/home/$my_LocalUSER/tmp/.pr0n/ to ~/tmp/$1/.pr0n/"
    unison -batch ~/tmp/.pr0n/ ~/tmp/$1/.pr0n/ && echo "Sync OK" && px-sshmount /home/px/tmp/$1/.pr0n
}

px-lan-check () { for ip in $(seq 1 10); do ping -c 1 192.168.0.$ip>/dev/null; if [ $? -eq 0 ] ; then echo "192.168.0.$ip UP" ; else echo "192.168.0.$ip DOWN" ; fi ; done }

px-wake-up-trackpad () {
    sudo rmmod psmouse
    sudo modprobe psmouse
}

px-commit-alten-pjs () {
    cd ~/Documents/Alten/svn/Support\ AGRESSO/pieces_jointes/
    svn status | grep '^?' | sed -e 's/^? *//' | xargs --no-run-if-empty -d '\n' svn add
}

px-dirsizes () { for dir in $1* ; do if [ -d $dir ] ; then du -hsL $dir ; fi ; done }

px-websearch () {
    firefox "https://duckduckgo.com/?q=$*"
}

px-find-this-and-do-that () {
    find . -name $1 -exec $2 '{}' \;
}

px-bkp () {
    cp -Rp $1 ${1%.*}.bkp-$(date +%y-%m-%d-%Hh%M).${1#*.}
}

px-ip () {
    ip -o -4 addr show | awk -F '[ /]+' '/global/ {print $4}'
    dig +short myip.opendns.com @resolver1.opendns.com
}

px-remind-me-this-in () {
    sleep $2
    zenity --info --text=$1
}

px-netstats () {
    echo -e "      $(ss -p | cut -f2 -sd\" | sort | uniq | wc -l) processes : $(ss -p | cut -f2 -sd\" | sort | uniq | xargs)
"
    lsof -P -i -n | uniq -c -w 10
    echo -e "
\t Distant connected IPs : \n $(netstat -an | grep ESTABLISHED | awk '{print $5}' | awk -F: '{print $1}' | sort | uniq -c | awk '{ printf("%s\t%s\t",$2,$1) ; for (i = 0; i < $1; i++) {printf("*")}; print "" }')
"
    if [ $1 ] ; then
        netstat -luntp
        echo -e "
\t Connected hostnames"
        for IP in $(netstat -an | grep ESTABLISHED | awk '{print $5}' | awk -F: '{print $1}' | sort | uniq); do host ${IP} | sed 's/\.in-addr.arpa domain name pointer/ \=\> /' ; done | grep -v '^;'
    else
        echo "use -a to see machine names (slow)"
    fi
}

px-notes () {
    if [ ! $1 ] ; then
echo -e "
################# NOTES
gnome-terminal --command byobu --maximize --hide-menubar
ESC DOT pops the last argument of the last command
DNS1 212.217.1.1 DNS2 .12 p.nom PPPoE / LLC
grep . * to cat a bunch of (small) files
ssh machine -L127.0.0.1:3306:127.0.0.1:3306
middleman build --clean && git commit -a -m 'new local build OK' && git push origin master
a && middleman build --clean && Commit 'deployed' && Push master
if ('$term' == emacs) set term=dumb
sudo ln -s /usr/lib/i386-linux-gnu/libao.so.4 /usr/lib/libao.so.2
sshfs name@server:/path/to/folder /path/to/mount/point

## Use px-notes \"this is a new note\" to add a note
"
fi
}

echo "kituu-commands loaded OK"
