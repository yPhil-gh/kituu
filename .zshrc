# Enable compsys completion.
autoload -U compinit
compinit


# Ye ol' Aliasses
alias ll="ls -alh"
alias la="ls -A"
alias lss="ls -la | grep $1"
alias hss="history | grep $1"
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

WORDCHARS="*?_-.[]~&;!#$%^(){}<>"

autoload -U colors && colors
PS1="%{$fg[green]%}%n%{$reset_color%}@%{$fg[red]%}%m %{$fg[yellow]%}%~ %{$reset_color%}%% "
