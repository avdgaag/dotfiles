# Edit command in EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

autoload -U colors
colors
setopt prompt_subst
PROMPT='%n@%m:%1~ %{$fg_bold[darkgrey]%}$(vcprompt)%# %{$reset_color%}'

# Enable coloured output from ls, etc
export CLICOLOR=1

# Load completions for Ruby, Git, etc.
autoload -U compinit
compinit

source ~/.bashrc
