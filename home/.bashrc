# Utilities
# ---------------------------------------------------------------------------------------

alias lsh='ls -lhGFr'
alias lsa='ls -lhaGFr'
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
alias postgres-start='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
alias postgres-stop='pg_ctl -D /usr/local/var/postgres stop -s -m fast'
alias redis-start='redis-server /usr/local/etc/redis.conf'

# Git
# ---------------------------------------------------------------------------------------
alias gs='git status -s --ignore-submodules=dirty'
alias gsf='git svn fetch'
alias gsr='git svn rebase'
alias gsd='git svn dcommit'
alias gb='git branch'
alias gc='git commit'
alias go='git checkout'
alias grd='git rm $(git ls-files -d)'   # git remove deleted
alias grc='git rebase --continue'
alias grs='git rebase --skip'
alias gra='git rebase --abort'
alias gw='git whatchanged --oneline'
alias gpom='git push origin master'
alias glog='git log --graph --pretty=format:"%C(yellow)%h%Creset -%C(red)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset" --abbrev-commit --decorate --branches -a'
alias gz='git archive -o snapshot.zip HEAD'
alias gm='git merge --no-ff --no-commit'
alias gmnff='git merge --no-ff'
alias gmc='git ls-files --unmerged | cut -f2 | uniq' # git merge conflicts
alias gtimelog='git --no-pager log --pretty=format:"%Cred%h%Creset - %Cgreen%cd%Creset - %s%Creset" --abbrev-commit --date=iso'

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
function rspec { if [ -e ./Gemfile.lock ]; then bundle exec rspec "$@"; else /usr/bin/env rspec "$@"; fi; }

# Rsync
alias sync='rsync -glpPrtvz --delete --exclude .svn --exclude .DS_Store --exclude .sass-cache'

export EDITOR="/usr/bin/vim"
export SVN_EDITOR=$EDITOR
export PATH="/opt/subversion/bin:/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:$PATH:/Users/arjan/bin:/Users/arjan/bin/dotfiles/scripts"
export PYTHONPATH=/usr/local/lib/python2.6/site-packages
export NODE_PATH=/usr/local/lib/node_modules:$NODE_PATH
