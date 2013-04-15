# My .zshrc - created 01 Jul 2012

#================================================
# format titles for screen and rxvt
#================================================
function title() {
  # escape '%' chars in $1, make nonprintables visible
  a=${(V)1//\%/\%\%}

  # Truncate command, and join lines.
  a=$(print -Pn "%20>...>$a" | tr -d "\n")

  case $TERM in
  screen)
    print -Pn "\ek$a $3\e\\"      # screen title (in ^A")
    ;;
  xterm*|*rxvt*)
    print -Pn "\e]2;$2 | $a $3\a" # plain xterm title
    ;;
  esac
}

# precmd is called just before the prompt is printed
function precmd() {
  title "zsh" "$USER@%m" "%55<...<%~"
}

# preexec is called just before any command line is executed
function preexec() {
  title "$(hostname):$1" "$USER@%m" "%35<...<%~"
}
# http://www.offensivethinking.org/data/dotfiles/zsh/zshrc

# #================================================
# # Tmux for every shell that is spawned
# #================================================
# if [ -z $TMUX ];
# then
#   tmux new || tmux attach;
# fi

# A TESTER
# set -g default-terminal "screen-256color"
# OR
# for tmux: export 256color
# [ -n "$TMUX" ] && export TERM=screen-256color

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
export UNISONLOCALHOSTNAME=$HOST

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

# bindkey '^e' emacs-backward-word

bindkey "^[^[[" forward-word      # Meta-RightArrow
bindkey "^[^[[" backward-word     # Meta-LeftArrow

# bindkey '^[[1;5C' emacs-forward-word
# bindkey '^[[1;5D' emacs-backward-word

bindkey "[C" emacs-forward-word   #control left
bindkey "[D" backward-word        #control right

## ZSH Funcs
insert_sudo () { zle beginning-of-line; zle -U "sudo " }
zle -N insert-sudo insert_sudo
bindkey "^[s" insert-sudo

insert_man () { zle beginning-of-line; zle -U "man " }
zle -N insert-man insert_man
bindkey "^[m" insert-man

insert_help () { zle end-of-line; zle -U " --help" }
zle -N insert-help insert_help
bindkey "^[h" insert-help

insert_localip () { zle end-of-line; zle -U " 192.168." }
zle -N insert-localip insert_localip
bindkey "^[l" insert-localip

# Generic funcs
. ~/.kituu-commands.sh

# Init
if (type "cowsay" > /dev/null && type "fortune" > /dev/null ); then cowsay `fortune -a` ; fi

# Prompt
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

# setopt extended_glob
# preexec () {
#     if [[ "$TERM" == "screen" ]]; then
# 	local CMD=${1[(wr)^(*=*|sudo|-*)]}
# 	echo -n "\ek$CMD\e\\"
#     fi
# }

# # Activations
# compinit
# colors

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

    # ###
    # # Screen title
    # if [[ "$TERM" == "screen" ]]; then
    #     PR_STITLE=$'%{\ekzsh\e\\%}'
    # else
    #     PR_STITLE=''
    # fi

    PR_STITLE=''

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
