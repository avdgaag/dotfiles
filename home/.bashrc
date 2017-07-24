# Utilities
# ---------------------------------------------------------------------------------------

alias emacs='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -n -a ""'

# Create and change into a new directory
function take() {
    mkdir -p "$*"
    cd "$*"
}

# Git
# ---------------------------------------------------------------------------------------
alias glog='git log --pretty=format:"%C(yellow)%h%C(reset) %C(green)%ar%C(reset) %C(bold blue)%an%C(reset) %C(red)%d%C(reset) %s" --graph --abbrev-commit --decorate'
alias gs='git status -b -s --ignore-submodules=dirty'

# Customize the prompt
export PS0="\[\033[0;34m\]\\$\[\033[0m\] "

export EDITOR=emacs

# Customize PATH
export PATH="/usr/local/heroku/bin:$PATH"
export PATH="/usr/local/bin:/usr/local/sbin:/usr/sbin:/sbin:$PATH"
export PATH="/Applications/Emacs.app/Contents/MacOS:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="./.git/safe/../../bin:$PATH"

# Enable colors in CLI
export CLICOLOR=1

# Rbenv
export RBENV_ROOT=/usr/local/var/rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
