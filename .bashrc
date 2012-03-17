# ===================================================================
# Kituu - the mildly over the top bash prompt - GPL3
# pX <hallucinet@online.fr>
# Time-stamp: <.bashrc - Sat 17-Mar-2012 08:47:39>
# ===================================================================

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Note : put the following in the path to launch emacsclient

# if [ "$(pidof emacs)" ] ; then
#     emacsclient "$@"
# else
#     emacs -mm "$@"
# fi

[ $UID != "0" ] && . ~/scripts/git-completion.bash
GIT_PS1_SHOWDIRTYSTATE=true

kituu_user=$(whoami)
kituu_processes=$(ps ax | wc -l | tr -d " ")

export ALTERNATE_EDITOR=emacs EDITOR=emacs

# Try to escape busybox
if [ -n "${BASH+x}" ] ; then
    [ $UID -eq 0 ] && PATH=$PATH:/sbin:/usr/sbin:/bin

    PATH=$PATH:/usr/local/include:/usr/bin/gnu
    PATH=/usr/local/bin:$PATH
    PATH=$PATH:~/scripts:~/bin
    shopt -s histappend
    shopt -s cdspell      # cd spell check (try cd /usr/bon)
    shopt -s cmdhist
    shopt -s checkwinsize
    shopt -s extglob      # rm !(everything_but_this_file)
    if [ "${BASH_VERSINFO}" -gt "3" ] ; then # We are using BASH > 4
        shopt -s globstar # (ls /home/*/tmp) !
        shopt -s autocd   # Just type dir name
    fi

# Set nice ls output colors
    if [ -x /usr/bin/dircolors ] ; then

        eval "`dircolors -b`"
        [ "$TERM" != "dumb" ] && eval "`dircolors -b`" && alias ls='ls --color=auto'
# Same for N900
    elif [ -x /usr/bin/gnu/dircolors ] ; then
        eval "`dircolors -b`"
        [ "$TERM" != "dumb" ] && eval "`dircolors -b`" && alias ls='/usr/bin/gnu/ls --color=auto'
    fi

else
    kituu_bash_version=`bash --version`
    echo "Enter BASH (${kituu_bash_version})"
    bash
fi

export HISTCONTROL=ignoreboth HISTFILESIZE=5000

# Ye ol' Aliasses
alias ll="ls -alh"
alias la="ls -A"
alias lss="ls -la | grep $1"
alias hss="history | grep $1"
alias mss="sudo cat /var/log/messages | grep $1"
alias uss="urpmq -y --summary"
alias rss="rpm -qa|grep -i"
alias rssi="rpm -qil"
alias MSG="sudo tail -f -n 40 /var/log/messages"
alias MSGh="sudo tail -f -n 40 /var/log/httpd/error_log"
alias U="urpmi"
alias screen="screen -h 5000"
alias Commit="git commit -am"
alias Push="git push origin"
alias Syncmail="offlineimap.py -o -u blinkenlights; reset"

# if hash vim 2>&- ; then
#     alias vi="vim"
# fi

pss () {
    ps aux | grep "[${1:0:1}]${1:1}"
}

bkp () {
    cp -Rp $1 ${1%.*}.bkp-$(date +%y-%m-%d-%Hh%M).${1#*.}
}

cleanup-turds () {
    find ./ -name "*~" -exec rm '{}' \; -print -or -name ".*~" -exec rm {} \; -print -or -name "#*#" -exec rm '{}' \; -print -or -name "*.swp" -exec rm '{}' \; -print
}

# Colors
export GREP_COLOR='1;33'
kituu_unset_color="\[\e[0m\]"         # unsets color to term's fg color

kituu_line_char="─"
kituu_trunc_symbol="--"
kituu_threshold_load=4                # CPU-meter threshold

kituu_host=$(hostname -s)

Kituu_bash_prompt_commands() {

    local kituu_time=$(date +%H:%M:%S)

    local kituu_load_average=$(uptime | awk -F 'load average:' '{ print $2 }' | (cut -d, -f1) | sed 's/ //g' | awk '{printf("%d\n",$1 + 0.5)}')

    # myint=`printf "%.0f\n" "$myfloat"`
    # local myint=$(echo "($myfloat+0.5)/1" | bc)

    case "$TERM" in
	xterm*|rxvt*|screen*|eterm-color)
local myChar=$(echo -e "\xE2\x80\xA2") # (•)
;;
*)
local myChar="|"
;;
esac

local kituu_load_color_lo="\e[0;91m"
local kituu_load_color_md="\e[0;31m"
local kituu_load_color_hi="\e[1;91m"
local kituu_load_color_bk="\E[5m"

let halfload=$kituu_threshold_load/2
if [ $kituu_load_average -gt $kituu_threshold_load ]; then
    kituu_load_meter=`echo -e ${kituu_load_color_hi}${myChar}${myChar}${kituu_load_color_bk}${myChar}`
    kituu_load_meter_size="nnn"
elif [ $kituu_load_average -gt $halfload ]; then
    kituu_load_meter=`echo -e ${kituu_load_color_md}${myChar}${myChar}`
    kituu_load_meter_size="nn"
else
    kituu_load_meter=`echo -e ${kituu_load_color_lo}${myChar}`
    kituu_load_meter_size="n"
fi

# this is your top info, right next to username@machine - add info like this - no wait don't touch anything, it's still WIP
kituu_info_up1=$kituu_time
kituu_info_up2_size=${kituu_load_meter_size}
kituu_info_up2=${kituu_load_meter}

# No git for root. Bad root.
if [ $UID != "0" ] ; then
kituu_git_status=`echo $(__git_ps1) | sed 's/^ *//'`
kituu_info_up3=${kituu_git_status}
fi

# The max. lengh of the pwd is half of the screen width - it should be automatically computed from the other elements (promptsize-path-some air)
if [ "${HOSTTYPE}" = "arm" ]; then
    pwdmaxlen=24
else
    # let pwdmaxlen=${COLUMNS}/2
(( pwdmaxlen = COLUMNS /2 ))
fi

# Do not edit below or you are likely to be eaten by a grue - believe me lil buddy you'll think that everything works but it secretly will not ;p
local dir=${PWD##*/}
    pwdmaxlen=$(( ( pwdmaxlen < ${#dir} ) ? ${#dir} : pwdmaxlen ))

    if [ "${BASH_VERSINFO}" -gt "3" ] ; then # We are using BASH > 4
	kituu_live_pwd=${PWD/#$HOME/\~}
    else
	kituu_live_pwd=${PWD}
    fi

    local pwdoffset=$(( ${#kituu_live_pwd} - pwdmaxlen ))

    if [ ${pwdoffset} -gt "0" ]
    then
	kituu_live_pwd=${kituu_live_pwd:$pwdoffset:$pwdmaxlen}
	kituu_live_pwd=${kituu_trunc_symbol}/${kituu_live_pwd#*/}
    fi

    promptsize=`echo -n "┌─($kituu_user@$kituu_host)($kituu_info_up1)($kituu_info_up2_size)$kituu_git_status($kituu_live_pwd)─┐"`

    let fillsize=${COLUMNS}-${#promptsize}
    kituu_fill=

    while [ "$fillsize" -gt "0" ]
    do
	kituu_fill=${kituu_fill}${kituu_line_char}
	let fillsize=${fillsize}-1
    done
}

kituu_bash_prompt() {
    local knc=$kituu_unset_color            # No regular color
    local klc=$kituu_line_color
    local kuc=$kituu_user_color
    local kpc=$kituu_user_color

# [ $UID -eq "0" ] && kuc=$kRc # root's color
    if [ $UID -eq "0" ]
    then
	local kuc="\[\e[1;31m\]"
	local kituu_user_symbol="#"
    else
	local kuc="\[\e[1;32m\]"
	local kituu_user_symbol="$"
    fi

# color path differently if not on my own machines
    if grep -q moe /etc/hosts; then kpc=$kuc; else kpc="\[\e[1;34m\]"; fi

# Return Smiley
    local kituu_smiley='$(if [[ $? -eq 0 ]]; then echo "\[\e[1;32m\]"":)"; else echo "\[\e[1;31m\]"":("; fi;)'

    case "$TERM" in
	"dumb")
	    PS1="> "
	    ;;
	xterm*|rxvt*|eterm*|screen*|linux*)
  	    PS1="${knc}┌─(${kuc}\u${knc}@\h)(\$kituu_info_up1)(\$kituu_info_up2${knc})\$kituu_info_up3${knc}\${kituu_fill}(${kpc}\${kituu_live_pwd}${knc})─┐\n└─(${kituu_smiley}${knc})─> $kituu_user_symbol "
  	    # PS1="${knc}┌─(${kuc}\u${knc}@\h)(\$kituu_info_up1)(\$kituu_info_up2${knc})\${kituu_fill}(${kpc}\${kituu_live_pwd}${knc})─┐\n└─(${kituu_smiley}${knc})─> $kituu_user_symbol "
	    # PS1='\[\033[32m\]\u@\h\[\033[00m\]:\[\033[34m\]\w\[\033[31m\]$(__git_ps1)\[\033[00m\]\$ '
	    ;;
	*)
	    PS1="> "
	    ;;
    esac
}

# export PROMPT_COMMAND="history -a;Kituu_bash_prompt_commands"
export PROMPT_COMMAND="Kituu_bash_prompt_commands"
kituu_bash_prompt
unset kituu_bash_prompt
