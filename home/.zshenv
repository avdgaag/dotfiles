# Tab completion moves to end of word
setopt ALWAYS_TO_END

# Automatically list choices on an ambiguous completion.
setopt AUTO_LIST

# Automatically use menu completion after the second consecutive
# request for completion, e.g. by pressing the tab key twice.
setopt AUTO_MENU

# Enable tab completion in the middle of a word
setopt COMPLETE_IN_WORD

# When listing files that are possible completions,
# show the type of each file with a trailing identifying mark.
setopt LIST_TYPES

# location of history
(( ${+HISTFILE} )) || export HISTFILE=~/.histfile

# number of lines kept in history
(( ${+HISTSIZE} )) || export HISTSIZE=10000

# number of lines saved in the history after logout
(( ${+SAVEHIST} )) || export SAVEHIST=10000

setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_NO_STORE
setopt HIST_NO_FUNCTIONS

# HASH_CMDS: Remember the location of each command the first time it runs.
# Subsequent invocations of the same command will use the saved location,
# thus avoiding a path search, thus making it faster to start a command.
setopt HASH_CMDS
setopt HASH_DIRS

# Allow comments even in interactive shells.
setopt INTERACTIVE_COMMENTS

# Enable output output flow control via start/stop characters,
# usually assigned to ^S/^Q, in the shell's editor.
setopt FLOW_CONTROL

setopt extendedglob nomatch notify

# Never beep
unsetopt BEEP
# CHASE_LINKS: Resolve symlinks to their values when changing directory.
# We unset it because symlinks are there for a reason; we want to see them.
unsetopt CHASE_LINKS
unsetopt CHASE_DOTS

# Homebrew's sbin
export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/Apple/usr/bin"

export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.asdf/shims:$PATH"

export EDITOR="emacsclient"
export VISUAL="emacsclient"
export PAGER=less

export FZF_DEFAULT_COMMAND='fd --type f --follow --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi

export LESS='-F -g -i -M -R -S -w -X -z-4'
