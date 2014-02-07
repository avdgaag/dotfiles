function! SetLastSpecCommand(spec)
  let s:last_spec_command = a:spec
endfunction

function! RunRspecFile()
  let l:spec = @%
  call SetLastSpecCommand(l:spec)
  call RunSpecs(l:spec)
endfunction

function! RunRspecLine()
  let l:spec = @% . ':' . line('.')
  call SetLastSpecCommand(l:spec)
  call RunSpecs(l:spec)
endfunction

function! RunSpecs(spec)
  let l:runner = PickRspecCommand()
  let l:command = substitute(l:runner, "{spec}", a:spec, "g")
  if $TMUX != ''
    call VimuxRunCommand(l:command)
  else
    execute "! clear && echo " . l:command . " && " . l:command
  endif
endfunction

function! RunLastRspec()
  if exists('s:last_spec_command')
    call RunSpecs(s:last_spec_command)
  endif
endfunction

function! RunRspec()
  let l:spec = 'spec'
  call SetLastSpecCommand(l:spec)
  call RunSpecs(l:spec)
endfunction

function! PickRspecCommand()
  if glob('.zeus.sock') != ''
    return 'zeus rspec {spec}'
  elseif glob('bin/rspec') != ''
    return 'bin/rspec {spec}'
  elseif glob('Gemfile') != '' && !exists('g:skip_bundle_exec')
    return 'bundle exec rspec {spec}'
  else
    return 'rspec {spec}'
  endif
endfunction

autocmd Filetype ruby nnoremap <buffer> <leader>k :call RunRspecFile()<cr>
autocmd Filetype ruby nnoremap <buffer> <leader>K :call RunRspecLine()<cr>
autocmd Filetype ruby nnoremap <buffer> <leader>l :call RunLastRspec()<cr>
autocmd Filetype ruby nnoremap <buffer> <leader>L :call RunRspec()<cr>
