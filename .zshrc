# My .zshrc - created 01 Jul 2012

# Enable compsys completion.
autoload -U compinit
autoload -U complist
autoload -U colors

# Options
setopt AUTO_CD
setopt COMPLETE_IN_WORD
setopt EXTENDED_HISTORY

# unsetopt EQUALS (hmm..)

setopt emacs
setopt AUTO_LIST
# setopt AUTO_MENU (overriden by MENU_COMPLETE)
setopt MENU_COMPLETE

setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
# hist_ignore_all_dups
# setopt hist_ignore_space
export HISTSIZE=10000
export HISTFILE="$HOME/.zsh_history"
export SAVEHIST=$HISTSIZE
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

# WORDCHARS="*?_-.[]~&;!#$%^(){}<>"
export WORDCHARS=''

# Ye ol' Aliasses
alias ls='ls -F --color=auto'
alias ll="ls -lha"
alias la="ls -A"
alias rm="rm -i"
alias cp="cp -i"
alias z="zile"

alias k="cd ~/.kituu/"
alias m="cd ~/.emacs.d/lisp/mail-bug/"

alias lss="ls -la | grep $1"
alias hss="history 0 | grep $1"
alias mss="sudo cat /var/log/messages | grep $1"
alias uss="urpmq -Y --summary"
alias rss="rpm -qa|grep -i"
alias rssi="rpm -qil"
alias MSG="sudo tail -f -n 40 /var/log/messages"
alias MSGh="sudo tail -f -n 40 /var/log/httpd/error_log"
alias U="urpmi"
# alias screen="screen -h 5000"
alias Commit="git commit -am"
alias Push="git push origin"
alias Syncmail="offlineimap.py -o -u blinkenlights; reset"
# alias Screen="screen -r $newest"
alias I="sudo apt-get install"
alias S="sudo apt-cache search"

## Funcs
# Alt-S inserts "sudo " at the start of line:
insert_sudo () { zle beginning-of-line; zle -U "sudo " }
zle -N insert-sudo insert_sudo
bindkey "^[s" insert-sudo

insert_man () { zle beginning-of-line; zle -U "man " }
zle -N insert-man insert_man
bindkey "^[m" insert-man

# ANSI color zebra output
zebra () {cat $1 | awk 'NR%2 == 1 {printf("\033[30m\033[47m%s\033[0m\n", $0); next}; 1'; }

# do a du -hs on each dir on current path
alias lsdir="for dir in *;do;if [ -d \$dir ];then;du -hsL \$dir;fi;done"

## BASH locate function for exact match
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

Find-this-and-do-that () {
    find . -name $1 -exec ls -l '{}' \;
}

pss () {
    ps aux | grep -i $1
}

bkp () {
    cp -Rp $1 ${1%.*}.bkp-$(date +%y-%m-%d-%Hh%M).${1#*.}
}

cleanup-turds () {
    find ./ -name "*~" -exec rm '{}' \; -print -or -name ".*~" -exec rm {} \; -print -or -name "#*#" -exec rm '{}' \; -print -or -name "*.swp" -exec rm '{}' \; -print
}

# Notes
# ssh machine -L127.0.0.1:3306:127.0.0.1:3306

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

    # autoload colors zsh/terminfo
    # if [[ "$terminfo[colors]" -ge 8 ]]; then
    #           colors
    # fi
    # for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    #           eval PR_$color='%{$terminfo[srg0]$fg[${(L)color}]%}'
    #           eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
    #           (( count = $count + 1 ))
    # done
    # PR_NO_COLOUR="%{$terminfo[sgr0]%}"

# http://lucentbeing.com/blog/that-256-color-thing/

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
	    kolor=242
	    ;;
	screen)
	    PR_TITLEBAR=$'%{\e_screen \005 (\005t) | %(!.-=[ROOT]=- | .)%n@%m:%~ | ${COLUMNS}x${LINES} | %y\e\\%}'
	    ;;
	linux)
	    kolor=reset
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

# 	PROMPT='$PR_SET_CHARSET$PR_STITLE${(e)PR_TITLEBAR}\
# $FG[$kolor]$PR_SHIFT_IN$PR_ULCORNER$PR_HBAR$PR_SHIFT_OUT(\
# $FG[214]%(!.%SROOT%s.%n)$FG[$kolor]@$FX[bold]$FG[214]%m$FX[reset]$FG[$kolor]:$FG[214]%l\
# $FG[$kolor])$PR_SHIFT_IN$PR_HBAR$PR_HBAR${(e)PR_FILLBAR}$PR_HBAR$PR_SHIFT_OUT(\
# $FG[214]%$PR_PWDLEN<...<%~%<<\
# $FG[$kolor])$PR_SHIFT_IN$PR_HBAR$PR_URCORNER$PR_SHIFT_OUT\

	PROMPT='$PR_SET_CHARSET$PR_STITLE${(e)PR_TITLEBAR}\
$PR_SHIFT_IN$PR_ULCORNER$PR_HBAR$PR_SHIFT_OUT(\
%(!.%SROOT%s.%n)@%m:%l\
)$PR_SHIFT_IN$PR_HBAR$PR_HBAR${(e)PR_FILLBAR}$PR_HBAR$PR_SHIFT_OUT(\
%$PR_PWDLEN<...<%~%<<\
)$PR_SHIFT_IN$PR_HBAR$PR_URCORNER$PR_SHIFT_OUT\

$PR_SHIFT_IN$PR_LLCORNER$PR_HBAR$PR_SHIFT_OUT(\
%(?..%?:)\
${(e)PR_APM}%D{%H:%M}\
) %(!..)\$ '

# This breaks in console
    # RPROMPT='\
# ($PR_YELLOW%D{%a,%b%d}$PR_WHITE)$PR_SHIFT_IN$PR_HBAR$PR_LRCORNER$PR_SHIFT_OUT$FX[reset]$FG[reset]'

	RPROMPT=''

	PS2='$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT(\
%_$PR_WHITE)$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT\
$PR_SHIFT_IN$PR_HBAR$PR_SHIFT_OUT '

    fi
}

setprompt

# PS1="%{$fg[green]%}%n%{$reset_color%}@%{$fg[red]%}%m %{$fg[yellow]%}%~ %{$reset_color%}%% "
