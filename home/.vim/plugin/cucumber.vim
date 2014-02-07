function! SetLastFeatureCommand(feature)
  let s:last_feature_command = a:feature
endfunction

function! RunCucumberFile()
  let l:feature = @%
  call SetLastFeatureCommand(l:feature)
  call RunFeatures(l:feature)
endfunction

function! RunCucumberLine()
  let l:feature = @% . ':' . line('.')
  call SetLastFeatureCommand(l:feature)
  call RunFeatures(l:feature)
endfunction

function! RunFeatures(feature)
  let l:runner = PickCucumberCommand()
  let l:command = substitute(l:runner, "{feature}", a:feature, "g")
  if $TMUX != ''
    call VimuxRunCommand(l:command)
  else
    execute "! clear && echo " . l:command . " && " . l:command
  endif
endfunction

function! RunLastCucumber()
  if exists('s:last_feature_command')
    call RunFeatures(s:last_feature_command)
  endif
endfunction

function! RunCucumber()
  let l:feature = 'features'
  call SetLastFeatureCommand(l:feature)
  call RunFeatures(l:feature)
endfunction

function! PickCucumberCommand()
  if glob('.zeus.sock') != ''
    return 'zeus cucumber {feature}'
  elseif glob('bin/cucumber') != ''
    return 'bin/cucumber {feature}'
  elseif glob('Gemfile') != '' && !exists('g:skip_bundle_exec')
    return 'bundle exec cucumber {feature}'
  else
    return 'cucumber {feature}'
  endif
endfunction

autocmd Filetype cucumber nnoremap <buffer> <leader>k :call RunCucumberFile()<cr>
autocmd Filetype cucumber nnoremap <buffer> <leader>K :call RunCucumberLine()<cr>
autocmd Filetype cucumber nnoremap <buffer> <leader>l :call RunLastCucumber()<cr>
autocmd Filetype cucumber nnoremap <buffer> <leader>L :call RunCucumber()<cr>
