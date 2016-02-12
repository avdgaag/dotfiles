# Utilities
# ---------------------------------------------------------------------------------------

alias vi='vim'
alias vims='vim `find . -type f | grep -v "^\./\."  | selecta`'
alias es='/usr/local/Cellar/emacs/24.5/bin/emacs --daemon'
alias emacs='/usr/local/Cellar/emacs/24.5/bin/emacsclient -c -n -a ""'
alias lsh='ls -lhGFr'
alias lsa='ls -lhaGFr'
alias nicedate='date "+%Y-%m-%d"'
alias nicedatetime='date "+%Y-%m-%d %H:%M"'
alias marked='open -a Marked'
alias bbundle='bundle check || bundle install | grep -v "Using "'
alias battery='ioreg -c AppleSmartBattery | awk '"'"'/MaxCapacity/ { max = $5 } /CurrentCapacity/ { current = $5 } END { printf("%.2f%%\n", current / max * 100) }'"'"
alias -g bb='`git rev-parse --abbrev-ref HEAD`'

function ffind() {
  find -E . -type f -regex ".*$@.*"
}

function vfind() {
  vim `find -E . -type f -regex ".*$@.*"`
}

# Create and change into a new directory
function take() {
    mkdir -p "$*"
    cd "$*"
}

function dash() {
  noglob open dash://"$*"
}

# Select a Tmux session to attach to
alias tmuxs='tmux attach -t `tmux ls | selecta | cut -f1 -d:`'

# Select a process to kill
alias kills='kill `ps aux | selecta | awk '"'"'{print $2 }'"'"'`'

# Working with these dotfiles made easier
alias reload='source ~/.bashrc'
alias ea='emacs ~/.bashrc && reload' # Edit aliases

# Pretty printing files
function pp() {
  pygmentize -O 'bg=dark,style=vim' -f terminal256 "$1" 
}

# Highlight clipboard in RTF for use in Keynote
function pk() {
  pbpaste | pygmentize -O 'style=friendly,fontface=Menlo' -f rtf $@ | sed 's/\f0/\f0\\fs80/g' | pbcopy
}

# Git
# ---------------------------------------------------------------------------------------
alias ga='git commit -a --amend --no-edit'
alias gap='git add -p'
alias gb='git branch'
alias gbc='git rev-parse --abbrev-ref HEAD'
alias gbs='git checkout `git branch --all --remotes | sed "s/..//" | rev | cut -d "/" -f 1 | rev | selecta`'
alias gc='git commit --verbose'
alias gca='git commit --all --verbose'
alias gd='git diff'
alias gdd='git difftool'
alias gds='git diff -w --staged'
alias gdw='git diff --word-diff'
alias gdws='git diff --staged --word-diff'
alias gfo='git fetch origin --prune'
alias gpo='git pull --prune'
alias gl='git log --pretty=format:"%C(yellow)%h%C(reset)|%C(bold blue)%an%C(reset)|%s" | column -s "|" -t | less -FXRS'
alias glr='git log --pretty="format:* %s" --merges --grep "pull request" | sed -e "s/Merge pull request #[0-9]\{1,\} from kabisaict\///" -e "s/_/ /g"'
alias glog='git log --pretty=format:"%C(yellow)%h%C(reset) %C(green)%ar%C(reset) %C(bold blue)%an%C(reset) %C(red)%d%C(reset) %s" --graph --abbrev-commit --decorate'
alias gm='git merge --no-ff --no-commit'
alias gmc='git ls-files --unmerged | cut -f2 | uniq' # git merge conflicts
alias gmf='git commit -F .git/MERGE_MSG'
alias gmnff='git merge --no-ff'
alias go='git checkout'
alias gp='git push'
alias gpf='git push --force-with-lease'
alias gpom='git push origin master'
alias gpp='git push -u origin `git rev-parse --abbrev-ref HEAD`'
alias gra='git rebase --abort'
alias grc='git rebase --continue'
alias grd='git rm $(git ls-files -d)'   # git remove deleted
alias grs='git rebase --skip'
alias grq='git rebase --interactive --autosquash'
alias gs='git status -b -s --ignore-submodules=dirty'
alias gsd='git svn dcommit'
alias gsf='git svn fetch'
alias gsr='git svn rebase'
alias gtimelog='! git --no-pager log --pretty=format:"%C(red)%h%C(reset){%C(green)%cd%C(reset){%C(bold blue)%an%C(reset){%s" --date=iso | column -t -s"{" | less -FXRS'
alias gw='git whatchanged --oneline'
alias gz='git archive -o snapshot.zip HEAD'

# Commit pending changes and quote all args as message
function gg() {
    git commit -v -a -m "$*"
}

# Bundler
alias be='bundle exec'

# Add ./bin to PATH to use bundler binstubs
alias binstubs='export PATH=./bin:$PATH'

# Rake
alias rakes='rake `rake -T | selecta | awk "{ print $2 }"`'
alias rake='noglob rake'

# Homebrew
alias bup='brew update && brew upgrade --all'
