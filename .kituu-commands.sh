# Aliases
alias rm="rm -i"
alias cp="cp -i"

if [[ ! $HOSTNAME == "N900" ]] ; then
    alias grep="grep -nIs --color"
    alias ls="ls --color"
fi

alias ll="ls -lha"
alias la="ls -A"

alias k="cd ~/.kituu/"
alias m="cd ~/.emacs.d/lisp/mail-bug/"
alias a="cd /var/www/adamweb/git.adamweb"
alias t="cd ~/tmp"
alias s="cd ~/src"

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

alias orgsync="cd ~/.org && git-sync.sh "

alias gitlog="git log --pretty=format:'%Cred%h%Creset | %C(yellow)%ad%Creset | %C(bold blue)%an%Creset - %s - %C(yellow)%d%Creset'"
# Commands

function ssh () {
    if [ $# -eq 1 ] ; then
        tmux rename-window `echo $1 | sed 's/.*@//g' | sed 's/.local//g'`
    fi
    command ssh $*
}

px-broadcast-mic () {
    arecord -f dat | ssh -C $1 aplay -f dat
}

px-ram-dump () {
    sudo cat /proc/kcore | strings | awk 'length > 20' | less
}

px-bandwidth-monitor () {
    [[ $# -eq 0 ]] && NIC="eth0" || NIC=$1
    while [ /bin/true ] ; do OLD=$NEW; NEW=`cat /proc/net/dev | grep $NIC | tr -s ' ' | cut -d' ' -f "3 11"`; echo $NEW $OLD | awk '{printf("\rin: % 9.2g\t\tout: % 9.2g", ($1-$3)/1024, ($2-$4)/1024)}'; sleep 1; done
}

px-flight_status() { if [[ $# -eq 3 ]];then offset=$3; else offset=0; fi; curl "http://mobile.flightview.com/TrackByRoute.aspx?view=detail&al="$1"&fn="$2"&dpdat=$(date +%Y%m%d -d ${offset}day)" 2>/dev/null |html2text | \grep ":"; }

px-guitar-tuner () {
    for N in E2 A2 D3 G3 B3 E4;do play -n synth 4 pluck $N repeat 2;done
}

px-what-is-this-program-doing-now () {
    diff <(lsof -p `pidof $1`) <(sleep 5; lsof -p `pidof $1`)
}

md () {
    mkdir -p $1
    cd $1
}

px-sshmount () {
    if [ ! $(grep "fuse.*$USER" /etc/group) ] ; then sudo gpasswd -a $USER fuse && echo "$0 : added $USER to group fuse" ; fi
    if [ "$#" -eq "1" ] ; then
        fusermount -u $1 && echo "$0 : Unmounted $1"
    else
        if  [ -w $2 ] ; then
            sshfs -o idmap=user $1 $2
        else
            echo "$0 : $2 is not writable"
        fi
    fi
}

px-vnc () {
    \ssh -f -L 5900:127.0.0.1:5900 $1 "x11vnc -scrollcopyrect -noxdamage -localhost -nopw -once -display :0" ; vinagre 127.0.0.1:5900
}

px-update-N900 () {
    rm .bashrc .kituu-commands.sh -f
    wget --no-check-certificate -nc https://github.com/xaccrocheur/kituu/raw/master/.kituu-commands.sh https://github.com/xaccrocheur/kituu/raw/master/.bashrc
    bash
}



px-lan-scan () {
    LOCAL_IP=$(ip -o -4 addr show | awk -F '[ /]+' '/global/ {print $4}')
    MASK="${LOCAL_IP:0:10}"
    GATEWAY=$(route -n | \grep '^0.0.0.0' | awk '{print $2}')
    if [ $1 ] ; then range=$1 ; else range="10" ; fi

    for num in $(seq 1 ${range}) ; do
        IP=$MASK$num
        if [[ $IP == $GATEWAY ]] ; then MACHINE="gateway" ; else MACHINE=$(avahi-resolve-address $IP 2>/dev/null | sed -e :a -e "s/$IP//g;s/\.[^>]*$//g;s/^[ \t]*//") ; fi
        ping -c 1 $IP>/dev/null
        if [ $? -eq 0 ] ; then
            echo -e "UP    $IP ($MACHINE)" ; else
            echo -e "DOWN  $IP"
        fi
    done
}

px-wake-up-trackpad () {
    sudo rmmod psmouse
    sudo modprobe psmouse
}

px-commit-alten-pjs () {
    cd ~/Documents/Alten/svn/Support\ AGRESSO/pieces_jointes/
    svn status | grep '^?' | sed -e 's/^? *//' | xargs --no-run-if-empty -d '\n' svn add
}

px-dirsizes () { for DIR in $1* ; do if [ -d $DIR ] ; then du -hsL $DIR ; fi ; done }

px-websearch () {
    firefox "https://duckduckgo.com/?q=$*"
}

function google () {
    u=`perl -MURI::Escape -wle 'print "http://google.com/search?q=". uri_escape(join " ",  @ARGV)' $@`
    links $u
}

px-find-this-and-do-that () {
    find . -name $1 -exec $2 '{}' \;
}

px-bkp () {
    cp -Rp $1 ${1%.*}.bkp-$(date +%y-%m-%d-%Hh%M).${1#*.}
}

px-ip () {
    echo -e "Local:   $(ip -o -4 addr show | awk -F '[ /]+' '/global/ {print $4}')"
    echo -e "distant: $(dig +short myip.opendns.com @resolver1.opendns.com)"
}

px-remind-me-this-in () {
    sleep $2
    zenity --info --text=$1
}

px-netstats () {
    if hash ss 2>/dev/null; then
        echo -e "      $(ss -p | cut -f2 -sd\" | sort | uniq | wc -l) processes : $(ss -p | cut -f2 -sd\" | sort | uniq | xargs) \n"
    fi
    lsof -P -i -n | uniq -c -w 10
    echo -e "
Distant connected IPs : \n $(netstat -an | grep ESTABLISHED | awk '{print $5}' | awk -F: '{print $1}' | sort | uniq -c | awk '{ printf("%s\t%s\t",$2,$1) ; for (i = 0; i < $1; i++) {printf("*")}; print "" }') \n"
    if [ $1 ] ; then
        netstat -luntp
        echo -e "
Connected hostnames"
        for IP in $(netstat -an | grep ESTABLISHED | awk '{print $5}' | awk -F: '{print $1}' | sort | uniq); do host ${IP} | sed 's/\.in-addr.arpa domain name pointer/ \=\> /' ; done | grep -v '^;'
    else
        echo "use -a to see machine names (slow)"
    fi
}

px-notes () {
    if [ ! $1 ] ; then
echo -e "
################# NOTES
ZSH : rm -rf ^survivorfile
rm -rf ^survivorfile
rm -f !(survivor_file)
0608853025
find . -type f -printf '%TY-%Tm-%Td %TT %p
' | sort
last arg of last command : !$
zdump Africa/Morocco Europe/Paris
tar -tf <file.tar.gz> | xargs rm -r
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
else
        sed -i '/^################# NOTES/a '$1'' ~/.kituu/.kituu-commands.sh && k && Commit "New note : $1" && Push master && cd -
fi
}

echo "kituu-commands loaded OK"
