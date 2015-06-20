# My .zshrc - created 01 Jul 2012

#colourful man pages
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

[ -n "$TMUX" ] && export TERM=screen-256color

# format titles for screen and rxvt
function title() {
  # escape '%' chars in $1, make nonprintables visible
    b=$1
    x="zib"

  a=${(V)1//\%/\%\%}

  # Truncate command, and join lines.
  a=$(print -Pn "%8>...>$a" | tr -d "\n")

  b=$(print -Pn "%8>...>$b" | tr -d "\n")

  case $TERM in
  screen*)
    # print -Pn "\ek$b $3\e\\"      # screen title (in ^A")
    print -Pn "\ek$b \e\\"      # screen title (in ^A")
    ;;
  xterm*|*rxvt*)
    print -Pn "\e]2;$2 | $a $3\a" # plain xterm title
    ;;
  esac
}

# precmd is called just before the prompt is printed
function precmd() {
  # title "zsh" "$USER@%m" "%55<...<%~"
  # title "$(hostname):$1" "$USER@%m" "%15<...<%~"
  # title "$(hostname):$1" "$2" "%15<...<%~"
  title "$(hostname):$1" "$2" "%15<...<%~"

}

# preexec is called just before any command line is executed
function preexec() {
  title "$(hostname):$1" "$USER@%m" "%15<...<%~"
}
# (http://www.offensivethinking.org/data/dotfiles/zsh/zshrc)

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

# See EOF for notes
# Enable compsys completion.
autoload -U compinit && compinit

# Commodities
setopt auto_cd
setopt complete_in_word
setopt emacs
# setopt auto_list
# implied by menu_complete
# setopt auto_menu
setopt menu_complete
setopt extended_glob
setopt no_bare_glob_qual

# HISTORY
# Implied by SHARE_HISTORY
# setopt INC_APPEND_HISTORY
setopt share_history
# timestamp
setopt extended_history
# this is default but hey
setopt hist_save_by_copy
setopt hist_ignore_dups # Do not write events to history that are duplicates of previous events
setopt hist_save_no_dups
setopt hist_find_no_dups # When searching history don't display results already cycled through twice
setopt hist_expire_dups_first
setopt hist_ignore_space # remove command line from history list when first character on the line is a space
setopt bang_hist
setopt append_history # Allow multiple terminal sessions to all append to one zsh command history
setopt inc_append_history # Add comamnds as they are typed, don't wait until shell exit
setopt hist_expire_dups_first # when trimming history, lose oldest duplicates first

# larger than SAVEHIST to accomodate dups
export HISTSIZE=10500
export SAVEHIST=10000
export HISTFILE="$HOME/.zsh_history"
export UNISONLOCALHOSTNAME=$(hostname)

setopt -o sharehistory
setopt list_ambiguous
setopt completealiases
setopt HIST_VERIFY

bindkey '^[[1;5D' backward-word
bindkey '^[[1;5C' forward-word

# bindkey "\e[Z" reverse-menu-complete # Shift+Tab in completion menu
bindkey '^[[3;3~' kill-word # alt-del kills word forward
bindkey "^B" backward-kill-line

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
zstyle ':completion:*' menu select=2
zstyle ':completion:*' completer _expand _complete _ignored _approximate
# zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
# zstyle ':completion:*:descriptions' format '%U%F{yellow}%d%f%u'
# ===== Completion

# compdef pkill=kill
# compdef pkill=killall
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

insert_ip () { zle end-of-line; zle -U " 192.168." }
zle -N insert-ip insert_ip
bindkey "^[i" insert-ip

insert_local () { zle end-of-line; zle -U ".local" }
zle -N insert-local insert_local
bindkey "^[l" insert-local

# Generic funcs
. ~/.kituu-commands.sh

# Init
zdump Africa/Morocco Europe/Paris

# if [ -d ~/.org ] ; then cd ~/.org && git-sync.sh ; fi

if (type "cowsay" > /dev/null && type "fortune" > /dev/null ); then
    cowsay `fortune -a`
else
    if [[ -x "$HOME/scripts/cowsay.pl" ]]
    then
        perl $HOME/scripts/cowsay.pl dlfp
    fi
fi

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

    if [[ "$HOST" = "meg" ]]; then
        HOSTCOLOR=$PR_RED
    else
        HOSTCOLOR=$PR_BLUE
    fi

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
$PR_GREEN%(!.%SROOT%s.%n)$PR_NO_COLOUR@$HOSTCOLOR%m$PR_NO_COLOUR:$PR_GREEN%l\
$PR_NO_COLOUR)$PR_SHIFT_IN$PR_HBAR$PR_HBAR${(e)PR_FILLBAR}$PR_HBAR$PR_SHIFT_OUT(\
$PR_GREEN%$PR_PWDLEN<...<%~%<<\
$PR_NO_COLOUR)$PR_SHIFT_IN$PR_HBAR$PR_URCORNER$PR_SHIFT_OUT\

$PR_SHIFT_IN$PR_LLCORNER$PR_HBAR$PR_SHIFT_OUT(\
%(?..$PR_LIGHT_RED%?:)\
${(e)PR_APM}$PR_NO_COLOUR%D{%H:%M}\
) %(!..)⚡$PR_NO_COLOUR '

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
