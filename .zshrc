# My .zshrc - created 01 Jul 2012

# See EOF for notes
# Enable compsys completion.
autoload -U compinit
autoload -U complist
autoload -U colors

# Commodities
setopt AUTO_CD
setopt COMPLETE_IN_WORD
setopt emacs
setopt AUTO_LIST
# Implied by MENU_COMPLETE
# setopt AUTO_MENU
setopt MENU_COMPLETE


# HISTORY

# Implied by SHARE_HISTORY
# setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
# timestamp
setopt EXTENDED_HISTORY
# This is default but hey
setopt HIST_SAVE_BY_COPY
setopt HIST_IGNORE_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_SPACE
setopt BANG_HIST

# larger than SAVEHIST to accomodate dups
export HISTSIZE=10500
export SAVEHIST=10000
export HISTFILE="$HOME/.zsh_history"
setopt -o sharehistory
setopt list_ambiguous
setopt completealiases
setopt HIST_VERIFY

bindkey ';5D' backward-word
bindkey ';5C' forward-word
bindkey "\e[Z" reverse-menu-complete # Shift+Tab in completion menu
bindkey '^[[3;3~' kill-word # alt-del kills word forward

# Path
PATH=$PATH:~/scripts:~/bin
# PATH=/usr/local/bin:$PATH

# GNU Colors 否则自动补全时候选菜单中的选项不能彩色显示
# [ -f /etc/DIR_COLORS ] && eval $(dircolors -b /etc/DIR_COLORS)
# export ZLSCOLORS="${LS_COLORS}"

# case-insensitive (uppercase from lowercase) completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# process completion
zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=36=31"

# known hosts completion
# zstyle -e ':completion::*:hosts' hosts 'reply=($(sed -e "/^#/d" -e "s/ .*\$//" -e "s/,/ /g" /etc/ssh_known_hosts(N) ~/.ssh/known_hosts(N) 2>/dev/null | xargs) $(grep \^Host ~/.ssh/config(N) | cut -f2 -d\  2>/dev/null | xargs))'

# zstyle
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' menu select=2
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*:descriptions' format '%U%F{yellow}%d%f%u'

compdef pkill=kill
compdef pkill=killall
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:processes' command 'ps -au$USER'

WORDCHARS="*?_-.[]~&;!#$%^(){}<>"
export WORDCHARS=''

# Ye ol' Aliasses
# Builtins redefs
alias ls='ls -F --color=auto'
alias rm="rm -i"
alias cp="cp -i"
alias grep="grep --color"

alias ll="ls -lha"
alias la="ls -A"

alias k="cd ~/.kituu/"
alias m="cd ~/.emacs.d/lisp/mail-bug/"
alias a="cd /var/www/adamweb/git.adamweb"

alias pss='ps aux | grep $(echo $1 | sed "s/^\(.\)/[\1]/g")'
alias lss="ls -la | grep $1"
alias hss="history 0 | grep $1"
alias mss="sudo cat /var/log/messages | grep $1"
alias uss="urpmq -Y --summary"
alias rss="rpm -qa|grep -i"
alias rssi="rpm -qil"
alias MSG="sudo tail -f -n 40 /var/log/syslog"
alias MSGh="sudo tail -f -n 40 /var/log/httpd/error_log"
alias U="urpmi"
# alias screen="screen -h 5000"
alias Commit="git commit -am"
alias Push="git push origin"
alias Syncmail="offlineimap.py -o -u blinkenlights; reset"
# alias Screen="screen -r $newest"
alias I="sudo apt-get install"
alias S="sudo apt-cache search"
# alias px-sshmount="sshfs -o idmap=user"

px-sshmount () {
    if (! grep -q "fuse.*$USER" /etc/group) {
            sudo gpasswd -a $USER fuse
            echo "adding $USER to group fuse"
	}
	fusermount -u $2
	sshfs -o idmap=user $1 $2
}

## Funcs
# Alt-S inserts "sudo " at the start of line:
insert_sudo () { zle beginning-of-line; zle -U "sudo " }
zle -N insert-sudo insert_sudo
bindkey "^[s" insert-sudo

insert_man () { zle beginning-of-line; zle -U "man " }
zle -N insert-man insert_man
bindkey "^[m" insert-man

# Append " --help"
insert_help () { zle end-of-line; zle -U " --help" }
zle -N insert-help insert_help
bindkey "^[h" insert-help

alias pxip="ip -o -4 addr show | awk -F '[ /]+' '/global/ {print $4}'"

# ANSI color zebra output
px-zebra () { cat $1 | awk 'NR%2 == 1 {printf("\033[30m\033[47m%s\033[0m\n", $0); next}; 1'; }

px-wake-up-trackpad () { sudo rmmod psmouse && sudo modprobe psmouse }

px-commit-alten-pjs () { cd ~/Documents/Alten/svn/Support\ AGRESSO/pieces_jointes/ && svn status | grep '^?' | sed -e 's/^? *//' | xargs --no-run-if-empty -d '\n' svn add }

px-ip () {
    ip -o -4 addr show | awk -F '[ /]+' '/global/ {print $4}'
    dig +short myip.opendns.com @resolver1.opendns.com
}

px-websearch () {
    firefox "https://duckduckgo.com/?q=$*"
}

px-remind-me-this-in () {
    sleep $2 && zenity --info --text=$1
}

px-netstats () {
    echo -e "      $(ss -p | cut -f2 -sd\" | sort | uniq | wc -l) processes : $(ss -p | cut -f2 -sd\" | sort | uniq | xargs)
"
    lsof -P -i -n | uniq -c -w 10
    echo -e "
\t Distant connected IPs : \n $(netstat -an | grep ESTABLISHED | awk '{print $5}' | awk -F: '{print $1}' | sort | uniq -c | awk '{ printf("%s\t%s\t",$2,$1) ; for (i = 0; i < $1; i++) {printf("*")}; print "" }')
"
    if [ $1 ] ; then
        for IP in $(netstat -an | grep ESTABLISHED | awk '{print $5}' | awk -F: '{print $1}' | sort | uniq); do host ${IP} | sed 's/\.in-addr.arpa domain name pointer/ \=\> /' ; done | grep -v '^;'
    else
        echo "use -q to see machine names (slow)"
    fi

}

px-tree () {
    ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'
}
# do a du -hs on each dir on current path
px-ls-dirsize () {
    for dir in $1*
    if [ -d $dir ] ; then
        du -hsL $dir
    fi
}

## exact match for locate
## Thanks Dark_Helmet : http://solarum.com/v.php?l=1149LV99
function flocate
{
  if [ $# -gt 1 ] ; then
    display_divider=1
  else
    display_divider=0
  fi

  current_argument=0
  total_arguments=$#
  while [ ${current_argument} -lt ${total_arguments} ] ; do
    current_file=$1
    if [ "${display_divider}" = "1" ] ; then
      echo "----------------------------------------"
      echo "Matches for ${current_file}"
      echo "----------------------------------------"
    fi

    filename_re="^\(.*/\)*$( echo ${current_file} | sed s%\\.%\\\\.%g )$"
    locate -r "${filename_re}"
    shift
    (( current_argument = current_argument + 1 ))
  done
}

px-find-this-and-do-that () {
    find . -name $1 -exec $2 '{}' \;
}


px-bkp () {
    cp -Rp $1 ${1%.*}.bkp-$(date +%y-%m-%d-%Hh%M).${1#*.}
}

# clear
# if ! type "ls" > /dev/null; then echo "plop" ; else echo "plip" ; fi

if (type "cowsay" > /dev/null && type "fortune" > /dev/null ); then cowsay `fortune -a` ; fi

# prompt
function precmd {
    local TERMWIDTH
    (( TERMWIDTH = ${COLUMNS} - 1 ))

    ###
    # Truncate the path

    PR_FILLBAR=""
    PR_PWDLEN=""

    local promptsize=${#${(%):---(%n@%m:%l)---()--}}
    local pwdsize=${#${(%):-%~}}

    if [[ "$promptsize + $pwdsize" -gt $TERMWIDTH ]]; then
	    ((PR_PWDLEN=$TERMWIDTH - $promptsize))
    else
	PR_FILLBAR="\${(l.(($TERMWIDTH - ($promptsize + $pwdsize)))..${PR_HBAR}.)}"
    fi
}

setopt extended_glob
preexec () {
    if [[ "$TERM" == "screen" ]]; then
	local CMD=${1[(wr)^(*=*|sudo|-*)]}
	echo -n "\ek$CMD\e\\"
    fi
}

# Activations
compinit
colors

setprompt () {
    ###
    # Need this so the prompt will work.

    setopt prompt_subst

    ###
    # See if we can use colors.

    typeset -Ag FX FG BG

    FX=(
        reset "[00m"
        bold "[01m" no-bold "[22m"
        italic "[03m" no-italic "[23m"
        underline "[04m" no-underline "[24m"
        blink "[05m" no-blink "[25m"
        reverse "[07m" no-reverse "[27m"
    )

    for color in {000..255}; do
        FG[$color]="[38;5;${color}m"
        BG[$color]="[48;5;${color}m"
    done


    autoload colors zsh/terminfo
    if [[ "$terminfo[colors]" -ge 8 ]]; then
        colors
    fi
    for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
        eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
        eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
        (( count = $count + 1 ))
    done
    PR_NO_COLOUR="%{$terminfo[sgr0]%}"

    ###
    # See if we can use extended characters to look nicer.

    typeset -A altchar
    set -A altchar ${(s..)terminfo[acsc]}
    PR_SET_CHARSET="%{$terminfo[enacs]%}"
    PR_SHIFT_IN="%{$terminfo[smacs]%}"
    PR_SHIFT_OUT="%{$terminfo[rmacs]%}"
    PR_HBAR=${altchar[q]:--}
    PR_ULCORNER=${altchar[l]:--}
    PR_LLCORNER=${altchar[m]:--}
    PR_LRCORNER=${altchar[j]:--}
    PR_URCORNER=${altchar[k]:--}

    ###
    # Titlebar text.

    case $TERM in
	xterm*)
	    PR_TITLEBAR=$'%{\e]0;%(!.-=*[ROOT]*=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\a%}'
	    ;;
	screen)
	    PR_TITLEBAR=$'%{\e_screen \005 (\005t) | %(!.-=[ROOT]=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\e\\%}'
	    ;;
	*)
	    PR_TITLEBAR=''
	    ;;
    esac

    ###
    # Screen title
    if [[ "$TERM" == "screen" ]]; then
	PR_STITLE=$'%{\ekzsh\e\\%}'
    else
	PR_STITLE=''
    fi

    # prompts
    if [[ $TERM == "dumb" ]]; then	# in emacs
    # for tramp to not hang, need the following. cf:
    # http://www.emacswiki.org/emacs/TrampMode
	unsetopt zle
	unsetopt prompt_cr
	unsetopt prompt_subst
	unfunction precmd
	unfunction preexec
	PS1='%(?..[%?])%!:%~%# '
    else

	# PROMPT="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m %{$fg[yellow]%}%~ %{$reset_color%}\$ "

	PROMPT='$PR_SET_CHARSET$PR_STITLE${(e)PR_TITLEBAR}\
$PR_SHIFT_IN$PR_ULCORNER$PR_HBAR$PR_SHIFT_OUT(\
$PR_GREEN%(!.%SROOT%s.%n)$PR_NO_COLOUR@$PR_RED%m$PR_NO_COLOUR:$PR_GREEN%l\
$PR_NO_COLOUR)$PR_SHIFT_IN$PR_HBAR$PR_HBAR${(e)PR_FILLBAR}$PR_HBAR$PR_SHIFT_OUT(\
$PR_GREEN%$PR_PWDLEN<...<%~%<<\
$PR_NO_COLOUR)$PR_SHIFT_IN$PR_HBAR$PR_URCORNER$PR_SHIFT_OUT\

$PR_SHIFT_IN$PR_LLCORNER$PR_HBAR$PR_SHIFT_OUT(\
%(?..$PR_LIGHT_RED%?:)\
${(e)PR_APM}$PR_NO_COLOUR%D{%H:%M}\
) %(!..)\$$PR_NO_COLOUR '

# 	PROMPT='$PR_SET_CHARSET$PR_STITLE${(e)PR_TITLEBAR}\
# $PR_SHIFT_IN$PR_ULCORNER$PR_HBAR$PR_SHIFT_OUT(\
# %(!.%SROOT%s.%n)@%m:%l\
# )$PR_SHIFT_IN$PR_HBAR$PR_HBAR${(e)PR_FILLBAR}$PR_HBAR$PR_SHIFT_OUT(\
# %$PR_PWDLEN<...<%~%<<\
# )$PR_SHIFT_IN$PR_HBAR$PR_URCORNER$PR_SHIFT_OUT\

# This breaks in console
    # RPROMPT='\
# ($PR_YELLOW%D{%a,%b%d}$PR_WHITE)$PR_SHIFT_IN$PR_HBAR$PR_LRCORNER$PR_SHIFT_OUT$PR_NO_COLOUR'

	RPROMPT=''
	PS2='$PR_NO_COLOUR$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_WHITE$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT(\
$PR_LIGHT_GREEN%_$PR_WHITE)$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT$PR_NO_COLOUR '

    fi
}

setprompt

# PS1="%{$fg[green]%}%n%{$reset_color%}@%{$fg[red]%}%m %{$fg[yellow]%}%~ %{$reset_color%}%% "

# NOTES
# ssh machine -L127.0.0.1:3306:127.0.0.1:3306
# middleman build --clean && git commit -a -m "new local build OK" && git push origin master
# a && middleman build --clean && Commit "deployed" && Push master
# if ("$term" == emacs) set term=dumb
# sudo ln -s /usr/lib/i386-linux-gnu/libao.so.4 /usr/lib/libao.so.2
