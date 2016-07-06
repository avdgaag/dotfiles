export EDITOR="/usr/local/Cellar/emacs/24.5/bin/emacsclient -c"
export ALTERNATE_EDITOR="/usr/local/Cellar/emacs/24.5/bin/emacs"
export VISUAL="/usr/local/bin/emacsclient -c -a emacs"
export PATH="$HOME/bin:/usr/local/bin:/usr/local/sbin:/usr/sbin:/sbin:$PATH"
export RBENV_ROOT=/usr/local/var/rbenv
export PAGER=/usr/bin/less
export LESS='-iMSFXR'
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export XML_CATALOG_FILES=/usr/local/etc/xml/catalog
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi
export PATH="/usr/local/heroku/bin:$PATH"
export PATH="./.git/safe/../../bin:$PATH"

