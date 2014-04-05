" rbfactor.vim - Simple refactorings for Ruby programs
" Maintainer:   Arjan van der Gaag <http://arjanvandergaag.nl>
" Version:      2.0

function! s:executeAndRestorePosition(cmd)
  silent execute "normal! ma<cr>"
  execute a:cmd
  silent execute "normal! `a<cr>"
endfunction

function! ConstantFromFilename()
  return substitute(substitute(expand('%:t:r'), '\(_\|^\)\@<=.', '\U&', 'g'), '_', '', 'g')
endfunction

function! ExtractVariable(mode)
  let name = input("Variable name: ", 'my_var')
  let cmd = 'normal! gv"fc' . name . 'O' . name . ' = '
  if a:mode ==# 'v'
    let cmd = cmd  . '"fp'
  elseif a:mode ==# 'V'
    let cmd = cmd . '"fpkJ'
  endif
  let cmd = cmd . '=='
  call s:executeAndRestorePosition(cmd)
endfunction

function! ExtractConstant(mode)
  let name = input("Constant name: ", 'NEW_CONSTANT')
  let cmd = 'normal gv"fc' . name . '==[[o' . name . ' = '
  if a:mode ==# 'v'
    let cmd = cmd  . '"fp'
  elseif a:mode ==# 'V'
    let cmd = cmd . '"fpkJ'
  endif
  let cmd = cmd . '==o'
  silent execute "normal! ma<cr>"
  execute cmd
  silent execute "normal! `a<cr>"
endfunction

function! AddParameter()
  let name = input("Parameter name: ", "arg")
  let line = getline('.')
  let cmd = ''
  if line =~ "\("
    let cmd = cmd . 'normal [mf)i, ' . name
  else
    let cmd = cmd. 'normal [mA' . name
  endif
  call s:executeAndRestorePosition(cmd)
endfunction

function! ExtractLet(mode)
  let name = input("Method name: ", 'new_method')
  let cmd = 'normal gv"fc' . name . '==?describe\|context?olet(:' . name .')'
  if a:mode ==# 'v'
    let cmd = cmd  . ' { "fp }'
  elseif a:mode ==# 'V'
    let cmd = cmd . ' do"fp==oend'
  endif
  call s:executeAndRestorePosition(cmd)
endfunction

function! ExtractMethod(mode)
  let name = input("Method name: ", 'new_method')
  let cmd = 'normal gv"fc' . name . '==[mOdef ' . name
  if a:mode ==# 'v'
    let cmd = cmd  . '"fp'
  elseif a:mode ==# 'V'
    let cmd = cmd . '"fp'
  endif
  let cmd = cmd . '==oend'
  call s:executeAndRestorePosition(cmd)
endfunction

function! InlineModifier()
  let cmd = 'normal! ^/unless\|if\|while\|until/"fDO"fp==j==oend=='
  call s:executeAndRestorePosition(cmd)
endfunction

vnoremap <leader>xm :call ExtractMethod(visualmode())<cr>
vnoremap <leader>xc :call ExtractConstant(visualmode())<cr>
vnoremap <leader>xv :call ExtractVariable(visualmode())<cr>
nnoremap <leader>xp :call AddParameter()<cr>
vnoremap <leader>xl :call ExtractLet(visualmode())<cr>
nnoremap <leader>xi :call InlineModifier()<cr>
