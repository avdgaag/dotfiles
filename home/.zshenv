export EDITOR="/usr/local/bin/vim"
export PATH="./.git/safe/../../bin:/usr/local/bin:/usr/local/sbin:/usr/sbin:/sbin:$PATH"
export RBENV_ROOT=/usr/local/var/rbenv
export PAGER=/usr/bin/less
export LESS='-iMSFXR'
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
