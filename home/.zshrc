# Edit command in EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey -v
bindkey -M vicmd v edit-command-line

bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward

autoload -U colors
colors

autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats "%{$fg[blue]%}%b%{$reset_color%} (%{$fg[red]%}%a%{$reset_color%}) %m%u%c%{$reset_color%}%{$fg[grey]%}%{$reset_color%}"
zstyle ':vcs_info:git*' formats "%{$fg[blue]%}%b%{$reset_color%}%m%u%c%{$reset_color%}%{$fg[grey]%}%{$reset_color%} "
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'
zstyle ':vcs_info:*' stagedstr "%{$fg[green]%}+%{$reset_color%}"
zstyle ':vcs_info:*' unstagedstr "%{$fg[red]%}+%{$reset_color%}"
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' enable git svn
precmd() {
  vcs_info
}

local prompt_with_exit_status="%(?,%{$fg[green]%}%#%{$reset_color%},%{$fg[red]%}%#%{$reset_color%})"
local colored_path="%{$fg_bold[black]%}%~%{$reset_color%}"
setopt prompt_subst
PROMPT='${colored_path}
${vcs_info_msg_0_}
${prompt_with_exit_status} '

# Indicate insert or command mode on right-hand side prompt
function zle-line-init zle-keymap-select {
    RPS1="%{$fg_bold[black]%}%(1j.%j.) %T%{$reset_color%}$del"
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# Enable coloured output from ls, etc
export CLICOLOR=1

# Load completions for Ruby, Git, etc.
autoload -Uz compinit
compinit

# autocomplete frequently used paths
setopt auto_cd
cdpath=($HOME/code)

source ~/.bashrc
source ~/.zshenv

# added by travis gem
source /Users/arjan/.travis/travis.sh
