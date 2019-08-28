export EDITOR=nvim
export VISUAL=nvim
export PAGER=less

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi

export LESS='-F -g -i -M -R -S -w -X -z-4'

