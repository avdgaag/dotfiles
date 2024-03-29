# if [ -x /usr/libexec/path_helper ]; then
#   eval `/usr/libexec/path_helper -s`
# fi

# Correctly display UTF-8 with combining characters.
# This code seems to come from the default OSX OhMyZsh.
if [ "$TERM_PROGRAM" = "Apple_Terminal" ]; then
  setopt combiningchars
fi

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
fi

unsetopt BGNICE
setopt CHECK_JOBS
unsetopt CORRECT
unsetopt CORRECT_ALL
setopt NO_BEEP
setopt SHARE_HISTORY

autoload -Uz compinit
compinit -C -d ~/.zcompdump

autoload -U promptinit
promptinit

autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line
bindkey -e

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
${vcs_info_msg_0_}${prompt_with_exit_status} '

export CLICOLOR=1

export ERL_AFLAGS="-kernel shell_history enabled"

alias gs='git status -b -s --ignore-submodules=dirty'
alias gb='git branch'
alias glog='git log --pretty=format:"%C(yellow)%h%C(reset) %C(green)%ar%C(reset) %C(bold blue)%an%C(reset) %C(red)%d%C(reset) %s" --graph --abbrev-commit --decorate'
alias go='git checkout'
alias gap='git add --patch'
alias gd='git diff'
alias gos='git branch | cut -c 3- | fzf | xargs git checkout'
alias gds='git diff --cached'
alias gc='git commit'
alias gf='git fetch origin --prune'
alias gpf='git push --force-with-lease'
alias ql='qlmanage -p "$@" &> /dev/null'

# Create and change into a new directory
function take() {
  mkdir -p "$*"
  cd "$*" || exit
}

export PATH="/usr/local/opt/asdf/bin:$PATH"
eval "$(asdf exec direnv hook zsh)"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
