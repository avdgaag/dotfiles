# Edit command in EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey -v
bindkey -M vicmd v edit-command-line

autoload -U colors
colors

autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f '
zstyle ':vcs_info:( sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'
zstyle ':vcs_info:*' enable git cvs svn
vcs_info_wrapper() {
  vcs_info
  if [ -n "$vcs_info_msg_0_" ]; then
    echo "%{$fg[grey]%}${vcs_info_msg_0_}%{$reset_color%}$del"
  fi
}

setopt prompt_subst
PROMPT='%n@%m:%1~ $(vcs_info_wrapper)%# '

# Indicate insert or command mode on right-hand side prompt
function zle-line-init zle-keymap-select {
    RPS1="${${KEYMAP/vicmd/N}/(main|viins)/}"
    RPS2=$RPS1
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# Enable coloured output from ls, etc
export CLICOLOR=1

# Load completions for Ruby, Git, etc.
autoload -U compinit
compinit

# autocomplete frequently used paths
setopt auto_cd
cdpath=($HOME/code)

source ~/.bashrc
source ~/.zshenv
