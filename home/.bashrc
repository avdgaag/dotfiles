alias emacs='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -n -a ""'
alias glog='git log --pretty=format:"%C(yellow)%h%C(reset) %C(green)%ar%C(reset) %C(bold blue)%an%C(reset) %C(red)%d%C(reset) %s" --graph --abbrev-commit --decorate'
alias gs='git status -b -s --ignore-submodules=dirty'
alias gb='git branch'
alias go='git checkout'
alias gap='git add --patch'
alias gos='git branch | cut -c 3- | fzf | xargs git checkout'
alias gd='git diff'
alias gds='git diff --cached'
alias gf='git fetch origin --prune'
alias gpf='git push --force-with-lease'
alias gc='git commit'
alias vim='nvim'
alias ia='open -a Ia\ Writer'
alias ql='qlmanage -p'

function p() {
  cd $(find ~/code -type d -maxdepth 1 | fzf)
}

# Create and change into a new directory
function take() {
  mkdir -p "$*"
  cd "$*" || exit
}

if [ -n "$NVIM_LISTEN_ADDRESS" ]; then
  alias nvim='echo "Will not nest neovim."'
fi

source ~/.ghcup/env

export FZF_DEFAULT_COMMAND='rg --files'
export ERL_AFLAGS="-kernel shell_history enabled"
export PS1="\[\033[0;34m\]\\$\[\033[0m\] "
export EDITOR='nvim'
export LANG=en_GB.UTF-8
export PATH="/usr/local/bin:/usr/local/sbin:/usr/sbin:/sbin:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"
export PATH="/Users/arjanvdgaag/Library/Python/3.7/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="./.git/safe/../../bin:$PATH"
export CLICOLOR=1
