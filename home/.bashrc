alias emacs='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -n -a ""'
alias glog='git log --pretty=format:"%C(yellow)%h%C(reset) %C(green)%ar%C(reset) %C(bold blue)%an%C(reset) %C(red)%d%C(reset) %s" --graph --abbrev-commit --decorate'
alias gs='git status -b -s --ignore-submodules=dirty'
alias gb='git branch'
alias go='git checkout'
alias gd='git diff'
alias gds='git diff --cached'
alias gf='git fetch origin --prune'
alias gpf='git push --force-with-lease'
alias gc='git commit'
alias vim='nvim'
alias ia='open -a Ia\ Writer'

# Create and change into a new directory
function take() {
  mkdir -p "$*"
  cd "$*" || exit
}

export FZF_DEFAULT_COMMAND='rg --files'
export PS1="\[\033[0;34m\]\\$\[\033[0m\] "
export EDITOR='nvim'
export LANG=en_GB.UTF-8
export PATH="/usr/local/bin:/usr/local/sbin:/usr/sbin:/sbin:$PATH"
export PATH="/$HOME/.cabal/bin:$PATH"
export PATH="/Users/arjan/Library/Python/3.7/bin:$PATH"
export PATH="/$HOME/.local/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="./.git/safe/../../bin:$PATH"
export CLICOLOR=1
