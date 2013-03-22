" Do not emulate vi
set nocompatible

call pathogen#infect()

" -------------------------------------
" General settings
set nobackup
set nowritebackup
set noswapfile
set hidden
set autoread
set history=100
set ttyfast
set lazyredraw
set backspace=indent,eol,start
set scrolloff=3
set wildmenu
set wildmode=longest,full
set wildignore+=tmp,.bundle,.sass-cache,.git,.svn,.hg,doc,coverage
set splitright
set splitbelow
set noerrorbells
set novisualbell
set t_vb=
set t_Co=256
set tm=500

" -------------------------------------
" Status line
function! StatusMode()
  if mode() == 'n'
    return 'N'
  elseif mode() == 'i'
    return 'I'
  elseif mode() ==? 'R'
    return 'R'
  elseif mode() ==? 'v'
    return 'V'
  else
    return '?'
  endif
endfun

set laststatus=2
set statusline=%-2{StatusMode()}    " current editor mode
set statusline+=%n                   " buffer number
set statusline+=\ %-.40F             " filename
set statusline+=\ %y                 " filetype
set statusline+=%m                   " modified flag
set statusline+=%r                   " read only flag
set statusline+=%q                   " quick fix
set statusline+=%=                   " left/right separator
set statusline+=%{v:register}\       " active register
set statusline+=%c,                  " cursor column
set statusline+=%l/%L                " cursor line/total lines
set showcmd                          " Show selection size or last command in command line
set ruler
set modelines=0

" -------------------------------------
" Colors, formatting and syntax highlighting
syntax on
filetype plugin indent on
set background=dark
colorscheme smyck
set nowrap
set synmaxcol=200 " Do not highlight long lines
set softtabstop=2
set tabstop=2
set shiftwidth=2
set expandtab
set autoindent
set smartindent
set nojoinspaces
set number
set numberwidth=4
set fillchars=vert:│
set encoding=utf-8
set list
set listchars=tab:\·\ ,trail:·,eol:¬

" -------------------------------------
" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase
set showmatch

" -------------------------------------
" Custom key mappings
nnoremap / /\v
vnoremap / /\v
nnoremap j gj
nnoremap k gk
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
cnoremap %% <C-R>=expand('%:h').'/'<cr>

nnoremap <leader><leader> <c-^>
nnoremap <leader>h :set invhls<CR>
nnoremap <leader>w :set nowrap<CR>
nnoremap <leader>y "*y
vnoremap <leader>y "*ygv
nnoremap <leader>p :set paste<CR>"*p<CR>:set nopaste<CR>
nnoremap <leader>P :set paste<CR>"*P<CR>:set nopaste<CR>
nnoremap <leader>sc <ESC>/\v^[<=>]{7}( .*\|$)<CR>
nnoremap <leader>sh :%s/\v:(\w+) \=\>/\1:/g<cr> " Replace Ruby 1.8 Hash syntax with 1.9 Hash syntax
nnoremap <leader>gr :topleft :split config/routes.rb<cr>
nnoremap <leader>gs :topleft :split db/schema.rb<cr>
nnoremap <leader>gg :topleft :split Gemfile<cr>
nnoremap <leader>gt :topleft :split TODO<cr> " Handy for keeping a TODO list in the project root

" -------------------------------------
" Macros

" Extract spec local variable into Rspec let(...) { ... }
let @l='jmlkdd?\vdescribe|context?p==I:^ys2w)Iletf=xys${`l' 
" Extract variable into method
let @e='mmIdef 2wdwioendmn:-2,.<:-2,.d?def ?%op' 
" Transform single-line conditional into conditional block
let @c='^/unless|if/DOpj>>oend' 

" -------------------------------------
" Plugins

" Vimux
nnoremap <Leader>vv :VimuxRunLastCommand<CR>
nnoremap <Leader>vp :VimuxPromptCommand<CR>
nnoremap <Leader>v[ :VimuxInspectRunner<CR>
nnoremap <Leader>vc :VimuxInterruptRunner<CR>
nnoremap <Leader>vq :VimuxCloseRunner<CR>
let g:VimuxHeight = '15'
let g:VimuxOrientation = "h"
let g:VimuxUseNearestPane = 1

" Tabular
nmap <leader>t= :Tabularize /=<CR>
vmap <leader>t= :Tabularize /=<CR>
nmap <leader>t: :Tabularize /:\zs /l0<CR>
vmap <leader>t: :Tabularize /:\zs /l0<CR>
nmap <leader>t, :Tabularize /,\zs /l0<CR>
vmap <leader>t, :Tabularize /,\zs /l0<CR>
nmap <leader>t> :Tabularize /=><CR>
vmap <leader>t> :Tabularize /=><CR>
nmap <leader>t" :Tabularize /"<CR>
vmap <leader>t" :Tabularize /"<CR>
nmap <leader>t{ :Tabularize /{<CR>
vmap <leader>t{ :Tabularize /{<CR>
nmap <leader>t\{ :Tabularize /\|<CR>
vmap <leader>t\{ :Tabularize /\|<CR>

" Switch
nnoremap - :Switch<cr>

" Git gutter
highlight clear SignColumn

function! StripTrailingWhitespace()
  silent exe "normal ma<CR>"
  let saved_search = @/
  %s/\s\+$//e
  silent exe "normal `a<CR>"
  let @/ = saved_search
endfunction

if has('autocmd')
  autocmd BufWritePre,FileWritePre *.rake,*.html,*.haml,*.rb,*.php,*.xml,*.erb,*.yml,*.scss,*.css,*.js,*.coffee call StripTrailingWhitespace()
  autocmd Filetype coffee,ruby,yaml,rake,rb,ru setlocal ts=2 sw=2 expandtab

  autocmd VimResized * wincmd =
  autocmd InsertLeave * set nopaste
  autocmd BufWritePost .vimrc source $MYVIMRC

  autocmd BufNewFile,BufRead {Gemfile,Guardfile,Capfile,Rakefile,Thorfile,config.ru,Vagrantfile,*.prawn} set ft=ruby
  autocmd BufNewFile,BufRead Gemfile.lock,Procfile set ft=yaml
  autocmd BufNewFile,BufRead *.json set ft=javascript

  autocmd FileType text,markdown,html,xhtml,eruby setlocal wrap linebreak nolist
  autocmd Filetype text,markdown set textwidth=80

  autocmd Filetype javascript nnoremap <Leader>r :!node %<CR>
  autocmd Filetype javascript nnoremap <Leader>vr :call VimuxRunCommand("clear; node " . bufname("%"))<CR>
  autocmd Filetype coffee nnoremap <Leader>r :CoffeeRun<CR>

  autocmd Filetype ruby nnoremap <Leader>r :!ruby %<CR>
  autocmd Filetype ruby nnoremap <Leader>vr :call VimuxRunCommand("clear; ruby " . bufname("%"))<CR>
  autocmd Filetype ruby nnoremap <Leader>k :!rspec %<CR>
  autocmd Filetype ruby nnoremap <Leader>K :exe "!bundle exec rspec %\:" . line(".")<cr>
  autocmd Filetype ruby nnoremap <Leader>vk :call VimuxRunCommand("clear; bundle exec rspec " . bufname("%"))<CR>
  autocmd Filetype ruby nnoremap <Leader>vK :call VimuxRunCommand("clear; bundle exec rspec " . bufname("%") . ":" . line("."))<CR>
  autocmd Filetype ruby nnoremap <Leader>vs :call VimuxRunCommand("clear; zeus rspec " . bufname("%"))<CR>
  autocmd Filetype ruby nnoremap <Leader>vS :call VimuxRunCommand("clear; zeus rspec " . bufname("%") . ":" . line("."))<CR>
  autocmd Filetype cucumber nnoremap <Leader>k :!bundle exec cucumber %<CR>
  autocmd Filetype cucumber nnoremap <Leader>K :exe "!bundle exec cucumber %\:" . line(".")<cr>
  autocmd Filetype cucumber nnoremap <Leader>vs :call VimuxRunCommand("clear; zeus cucumber " . bufname("%"))<CR>
  autocmd Filetype cucumber nnoremap <Leader>vS :call VimuxRunCommand("clear; zeus cucumber " . bufname("%") . ":" . line("."))<CR>
  autocmd Filetype cucumber nnoremap <Leader>vk :call VimuxRunCommand("clear; bundle exec cucumber " . bufname("%"))<CR>
  autocmd Filetype cucumber nnoremap <Leader>vK :call VimuxRunCommand("clear; bundle exec cucumber " . bufname("%") . ":" . line("."))<CR>

  autocmd User Rails Rnavcommand factory spec/factories/ -suffix=.rb
  autocmd User Rails Rnavcommand decorator app/decorators/ -suffix=_decorator.rb
  autocmd User Rails Rnavcommand concern app/concerns/ -suffix=.rb
endif

if executable("ack")
  set grepprg=ack\ -H\ --nogroup\ --nocolor\ --ignore-dir=tmp
endif

