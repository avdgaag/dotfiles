# Utilities
# ---------------------------------------------------------------------------------------

alias lsh='ls -lhGF'
alias lsa='ls -lhaGF'
alias nicedate='date "+%Y-%m-%d"'
alias nicedatetime='date "+%Y-%m-%d %H:%M"'
alias ffind='find -E . -type f -regex'

# Create and change into a new directory
function take() {
    mkdir -p "$*"
    cd "$*"
}

# Working with these dotfiles made easier
alias reload='source ~/.bashrc'
alias ea='vim ~/.bashrc && reload' # Edit aliases

# Other
alias apache-config='sudo vim /etc/apache2/httpd.conf'
alias apache-check='sudo apachectl configtest'
alias apache-restart='sudo apachectl graceful'
alias apache-vhosts='sudo vim /etc/apache2/extra/httpd-vhosts.conf'

# Git
# ---------------------------------------------------------------------------------------
alias git=hub # we use hub to add some github power to git
alias gs='git status -s --ignore-submodules=dirty'
alias gsf='git svn fetch'
alias gsr='git svn rebase'
alias gsd='git svn dcommit'
alias gb='git branch'
alias gc='git commit'
alias go='git checkout'
alias grd='git rm $(git ls-files -d)'   # git remove deleted
alias gw='git whatchanged --oneline'
alias gpom='git push origin master'
alias glog='git log --graph --pretty=oneline --abbrev-commit --decorate --branches -a'
alias gz='git archive -o snapshot.zip HEAD'

# Commit pending changes and quote all args as message
function gg() {
    git commit -v -a -m "$*"
}

# Subversion
# ---------------------------------------------------------------------------------------
alias surl="svn info | grep URL | grep -Eo 'http[a-zA-Z0-9:/@\.\-]+'"
alias srev='svn info | grep Revision'
alias ss='svn status --ignore-externals'
alias slsoc='svn log --verbose --stop-on-copy'
alias su='svn update --ignore-externals'
alias sroot='surl | sed -E "s/(trunk|tags|branches).*$//"'

# Commit pending changes and quote all args as message
function sc() {
  svn commit -m "$*"
}

# List directory contents from the root URL of the current project.
function sl() {
    dir=`sroot`$1
    svn list $dir
}

function sw() {
    dir=`sroot`"/"$1
    svn switch $dir
}

# Create and switch to a new subversion branch
function sb() {
    svn copy `surl` `sroot`"/branches/"$1
    svn switch `sroot`"/branches/"$1
    echo "Now at branch $1"
}

# Create a new tag from the current URL
function st() {
    svn copy `surl` `sroot`"/tags/"$1
    echo "Created new tag $tag_name"
}

function mate_with {
    file_pattern=$1
    find . -name $file_pattern -print0 | xargs -0 mate
}

# Bundler
alias be='bundle exec'

# Auto-detect if 'bundle exec' should be used when using nanoc or rake
function rake  { if [ -e ./Gemfile.lock ]; then bundle exec rake  "$@"; else /usr/bin/env rake  "$@"; fi; }
function nanoc { if [ -e ./Gemfile.lock ]; then bundle exec nanoc "$@"; else /usr/bin/env nanoc "$@"; fi; }

# Rsync
alias sync='rsync -glpPrtvz --delete --exclude .svn --exclude .DS_Store --exclude .sass-cache'

export EDITOR="/usr/bin/vim"
export SVN_EDITOR=$EDITOR
export PATH="/opt/subversion/bin:/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH:/Users/arjan/bin:/Users/arjan/bin/dotfiles/scripts"
export PYTHONPATH=/usr/local/lib/python2.6/site-packages
