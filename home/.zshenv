export EDITOR="/usr/local/bin/vim"
export PATH="/Users/arjan/bin:/usr/local/bin:/usr/local/sbin:/usr/local/share/npm/bin:$PATH"
export NODE_PATH=/usr/local/lib/node_modules:$NODE_PATH
export RBENV_ROOT=/usr/local/opt/rbenv
export PAGER=/usr/bin/less
export LESS='-iMSFXR'
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
eval "$(/Users/arjan/code/ag/bin/ag init -)"
