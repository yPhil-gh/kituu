# Enable compsys completion.
autoload -U compinit
compinit

WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

autoload -U colors && colors
PS1="%{$fg[green]%}%n%{$reset_color%}@%{$fg[red]%}%m %{$fg[yellow]%}%~ %{$reset_color%}%% "
