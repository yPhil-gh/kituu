# Aliases
alias ls='ls -F --color=auto'
alias rm="rm -i"
alias cp="cp -i"
alias grep="grep -RnIs --color=always"

alias ll="ls -lha"
alias la="ls -A"

alias k="cd ~/.kituu/"
alias m="cd ~/.emacs.d/lisp/mail-bug/"
alias a="cd /var/www/adamweb/git.adamweb"

alias pss='ps aux | \grep --color $(echo $1 | sed "s/^\(.\)/[\1]/g")'
alias mss="sudo cat /var/log/messages | grep $1"
alias uss="urpmq -Y --summary"
alias rss="rpm -qa|grep -i"
alias rssi="rpm -qil"
alias MSG="sudo tail -f -n 40 /var/log/syslog"
alias MSGh="sudo tail -f -n 40 /var/log/httpd/error_log"
alias U="urpmi"
alias Commit="git commit -am"
alias Push="git push origin"
alias Syncmail="offlineimap.py -o -u blinkenlights; reset"
alias I="sudo apt-get install"
alias S="sudo apt-cache search"
# Commands

ssh () {
    tmux rename-window `echo $1 | sed 's/.*@//g'`
    command ssh $1
}

md () { mkdir -p $1 && cd $1 }

px-sshmount () {
    if [ ! grep -q "fuse.*$USER" /etc/group ] ; then sudo gpasswd -a $USER fuse && echo "added $USER to group fuse" ; fi
    if [ ! -n "$2" ] ; then fusermount -u $1 && echo "Unmounted $1" ; else sshfs -o idmap=user $1 $2 ; fi
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

px-wake-up-trackpad () { sudo rmmod psmouse && sudo modprobe psmouse }

px-commit-alten-pjs () { cd ~/Documents/Alten/svn/Support\ AGRESSO/pieces_jointes/ && svn status | grep '^?' | sed -e 's/^? *//' | xargs --no-run-if-empty -d '\n' svn add }

px-dirtree () { ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/' }

px-vnc () { ssh -f -L 5900:127.0.0.1:5900 $1 "x11vnc -safer -localhost -nopw -once -display :0"; vinagre 127.0.0.1:5900 }

px-dirsizes () { for dir in $1* ; do if [ -d $dir ] ; then du -hsL $dir ; fi ; done }

px-ip () { ip -o -4 addr show | awk -F '[ /]+' '/global/ {print $4}' && dig +short myip.opendns.com @resolver1.opendns.com }

px-websearch () { firefox "https://duckduckgo.com/?q=$*" }

px-remind-me-this-in () { sleep $2 && zenity --info --text=$1 }

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
else
        sed -i '/^################# NOTES/a '$1'' ~/.kituu/.kituu-commands.sh && k && Commit "New note : $1" && Push master && cd -
fi
}

px-find-this-and-do-that () { find . -name $1 -exec $2 '{}' \; }

px-bkp () { cp -Rp $1 ${1%.*}.bkp-$(date +%y-%m-%d-%Hh%M).${1#*.} }

nameTerminal() {
    [ "${TERM:0:5}" = "xterm" ]   && local ansiNrTab=0
    [ "$TERM"       = "rxvt" ]    && local ansiNrTab=61
    [ "$TERM"       = "konsole" ] && local ansiNrTab=30 ansiNrWindow=0
        # Change tab title
    [ $ansiNrTab ] && echo -n $'\e'"]$ansiNrTab;$1"$'\a'
        # If terminal<a href="http://www.edmondscommerce.co.uk/magento-support"> support </a>separate window title, change window title as well
    [ $ansiNrWindow -a "$2" ] && echo -n $'\e'"]$ansiNrWindow;$2"$'\a'
}
