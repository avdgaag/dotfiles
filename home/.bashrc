alias emacs='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -n -a ""'
alias vim='nvim'
alias glog='git log --pretty=format:"%C(yellow)%h%C(reset) %C(green)%ar%C(reset) %C(bold blue)%an%C(reset) %C(red)%d%C(reset) %s" --graph --abbrev-commit --decorate'
alias gs='git status -b -s --ignore-submodules=dirty'
alias gb='git branch'
alias go='git checkout'
alias gd='git diff'
alias gds='git diff --cached'
alias gf='git fetch origin --prune'
alias gpf='git push --force-with-lease'
alias gc='git commit'

# Create and change into a new directory
function take() {
  mkdir -p "$*"
  cd "$*" || exit
}

export PS1="\[\033[0;34m\]\\$\[\033[0m\] "
export EDITOR=nvim
export VISUAL=nvim
export LANG=en_GB.UTF-8
export PATH="/usr/local/heroku/bin:$PATH"
export PATH="/usr/local/bin:/usr/local/sbin:/usr/sbin:/sbin:$PATH"
export PATH="/Applications/Emacs.app/Contents/MacOS:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="./.git/safe/../../bin:$PATH"
export CDPATH=.:~/code
export CLICOLOR=1
export RBENV_ROOT=/usr/local/var/rbenv

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
