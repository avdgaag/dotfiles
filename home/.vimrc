" Do not emulate vi
set nocompatible

" Force 256 colors
set t_Co=256

" Set up pathogen to load plugins
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" Regular Vim settings
" --------------------

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

" Status line
set laststatus=2
set statusline+=%-2{StatusMode()}    " current editor mode
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

" Line numbers 
set number
set numberwidth=4

" Scrolling
set sidescrolloff=2
set nowrap
set linebreak
set bs=indent,eol,start              " Intuitive Backspace
set scrolloff=3                      " Maintain more context around the cursor

" File navigation and splits
set hidden                           " Deal witg multiple buffers better
set autoread                         " Automatically update changes to a file when receiving focus
set fillchars=stl:─,stlnc:─,vert:│   " Use pretty characters for horizontal and vertical split bars
set sb                               " Always open new windows below/to the right
set spr

" Misc
set history=100                      " Keep a longer history
filetype plugin indent on            " Filetype detection
set title                            " Set terminal title
set ttyfast                          " fast connection!
set lazyredraw                       " Do not redraw during macros etc.



" Invisible characters
set encoding=utf-8                   " Always work with utf-8
set list
set listchars=tab:\·\ ,trail:·,eol:¬

" Colors
syntax on
set background=dark
colorscheme smyck
set synmaxcol=400                    " Do not highlight long lines

" Copy & Paste
noremap <leader>y "*y
noremap <leader>p :set paste<CR>"*p<CR>:set nopaste<CR>
noremap <leader>P :set paste<CR>"*P<CR>:set nopaste<CR>
vnoremap <leader>y "*ygv

" Swap files
set nobackup                         " Do not keep backup files. We use source control anyway.
set nowritebackup
set noswapfile

" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase
set showmatch

" Spaces, tabs and indenting
set softtabstop=2
set tabstop=2
set shiftwidth=2
set expandtab
set ai                               " Automatically set the indent of a new line
set si                               " Smart indent
set pastetoggle=<F2>                 " Toggle paste mode to reduce paste indent suckage
set nojoinspaces                     " Do not use two spaces after sentences

" Menu autocompletion
set wildmenu                         " Make file/command completion useful
set wildmode=list:longest,list:full
set wildignore+=tmp,.bundle,.sass-cache,.git,.svn,.hg,doc

" Custom keymaps for Vim commands
" -------------------------------

nnoremap <Space> :set cursorcolumn!<Bar>set cursorline!<CR> " Highlight the current line and column
nnoremap <Leader>E :e! ~/.vimrc<cr> " Fast editing and updating of the .vimrc (reloads on save)
nnoremap <Leader>h :set invhls <CR> " Hide search highlighting
nnoremap <Leader>w :set nowrap!<CR> " Toggle word wrap with \w

nnoremap / /\v
vnoremap / /\v

" use editor lines, not real lines
nnoremap j gj
nnoremap k gk

" Easy window navigation
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" Re-map arrow keys to do something useful, but not the usual navigation
" (force use of hjkl)
"
" Cycle up and down through buffers, left and right to indent or outdent
nnoremap <up>   :bp<cr>
nnoremap <down> :bn<cr>
nnoremap <silent> <Left> <<
nnoremap <silent> <Right> >>
vnoremap <silent> <Left> <
vnoremap <silent> <Right> >
inoremap <silent> <Left> <C-D>
inoremap <silent> <Right> <C-T>

" Search mappings
" ---------------

nnoremap <silent> <leader>cf <ESC>/\v^[<=>]{7}( .*\|$)<CR> " Find Git merge conflict markers
nnoremap <leader>rh :%s/\v:(\w+) \=\>/\1:/g<cr> " Replace Ruby 1.8 Hash syntax with 1.9 Hash syntax

" File navigation mappings
" ------------------------

" Quickly edit files from the same directory as the current file
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>e :edit %%
map <leader>] :cnext<cr>
map <leader>[ :cprev<cr>

" Some Rails-specific jump-to-file bindings
nnoremap <leader>gr :topleft :split config/routes.rb<cr>
nnoremap <leader>gs :topleft :split db/schema.rb<cr>
nnoremap <leader>gg :topleft :split Gemfile<cr>
nnoremap <leader>gn :topleft :split config/locales/nl.yml<cr>
nnoremap <leader>ge :topleft :split config/locales/en.yml<cr>
nnoremap <leader>gt :topleft :split TODO<cr> " Handy for keeping a TODO list in the project root

" Working with splits and buffers
nnoremap <leader>ts :topleft :split<cr>
nnoremap <Leader>n :topleft 30vsp.<cr>
nnoremap <leader><leader> <c-^>

" Macros
let @l='jmlkdd?\vdescribe|context?p==I:^ys2w)Iletf=xys${`l' " Extract spec local variable into Rspec let(...) { ... }

" Plugin settings and keymaps
" ---------------------------

" Command-T
nnoremap <silent> <C-b> :CommandTBuffer<CR>
nnoremap <silent> <C-t> :CommandT<CR>
let g:CommandTMaxHeight=20
let g:CommandTMatchWindowReverse=1
let g:CommandTAcceptSelectionSplitMap='<C-g>'

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
nmap <Leader>t= :Tabularize /=<CR>
vmap <Leader>t= :Tabularize /=<CR>
nmap <Leader>t: :Tabularize /:\zs /l0<CR>
vmap <Leader>t: :Tabularize /:\zs /l0<CR>
nmap <Leader>t, :Tabularize /,\zs /l0<CR>
vmap <Leader>t, :Tabularize /,\zs /l0<CR>
nmap <Leader>t> :Tabularize /=><CR>
vmap <Leader>t> :Tabularize /=><CR>
nmap <Leader>t" :Tabularize /"<CR>
vmap <Leader>t" :Tabularize /"<CR>
nmap <Leader>t{ :Tabularize /{<CR>
vmap <Leader>t{ :Tabularize /{<CR>

" Map switch.vim plugin to -
nnoremap - :Switch<cr>

" Remove trailing whitespace from code files on save
function! StripTrailingWhitespace()

  " store current cursor location
  silent exe "normal ma<CR>"
  " store the current search value
  let saved_search = @/


  " delete the whitespace (e means don't warn if pattern not found)
  %s/\s\+$//e

  " restore old cursor location
  silent exe "normal `a<CR>"
  " restore the search value
  let @/ = saved_search

endfunction

" Autocommands
" ------------

if has('autocmd')

  " Remove trailing whitespace from various files
  autocmd BufWritePre,FileWritePre *.rake,*.html,*.haml,*.rb,*.php,*.xml,*.erb,*.yml,*.scss,*.css call StripTrailingWhitespace()

  " Use 2 spaces for tabs in ruby and associated langs
  autocmd Filetype coffee,ruby,yaml,rake,rb,ru setlocal ts=2 sw=2 expandtab
  autocmd BufNewFile,BufRead {Gemfile,Guardfile,Capfile,Rakefile,Thorfile,config.ru,Vagrantfile,*.prawn} set ft=ruby
  autocmd BufNewFile,BufRead Gemfile.lock,Procfile set ft=yaml
  autocmd BufNewFile,BufRead *.json set ft=javascript

  " Run ruby files using \r
  autocmd Filetype ruby,rb nnoremap <Leader>r :!ruby %<CR>
  autocmd Filetype ruby,rb nnoremap <Leader>vr :call VimuxRunCommand("clear; ruby " . bufname("%"))<CR>
  autocmd BufNewFile,BufRead,BufEnter *_spec.rb nnoremap <Leader>k :!bundle exec rspec %<CR>
  autocmd BufNewFile,BufRead,BufEnter *_spec.rb nnoremap <Leader>K :exe "!bundle exec rspec %\:" . line(".")<cr>
  autocmd BufNewFile,BufRead,BufEnter *_spec.rb nnoremap <Leader>vk :call VimuxRunCommand("clear; bundle exec rspec " . bufname("%"))<CR>
  autocmd BufNewFile,BufRead,BufEnter *_spec.rb nnoremap <Leader>vK :call VimuxRunCommand("clear; bundle exec rspec " . bufname("%") . ":" . line("."))<CR>
  autocmd BufNewFile,BufRead,BufEnter *_spec.rb nnoremap <Leader>vs :call VimuxRunCommand("clear; spin push " . bufname("%"))<CR>
  autocmd BufNewFile,BufRead,BufEnter *_spec.rb nnoremap <Leader>vS :call VimuxRunCommand("clear; spin push " . bufname("%") . ":" . line("."))<CR>

  " Run JS files with Node
  autocmd Filetype javascript nnoremap <Leader>r :!node %<CR>
  autocmd Filetype javascript nnoremap <Leader>vr :call VimuxRunCommand("clear; node " . bufname("%"))<CR>

  " Set up some build commands for Coffeescript. Compile the entire file or
  " a selection with \b
  autocmd Filetype coffee nnoremap <Leader>b :CoffeeCompile<CR>
  autocmd Filetype coffee vnoremap <Leader>b :CoffeeCompile<CR>

  " Enable soft-wrapping for text files
  autocmd FileType text,markdown,html,xhtml,eruby setlocal wrap linebreak nolist

  " Auto-resise windows when resizing
  autocmd VimResized * wincmd =

  " Folding on indent for HAML and coffee-script files
  autocmd BufNewFile,BufReadPost *.{coffee,haml} setl foldmethod=indent nofoldenable

  " Auto-disable paste mode when leaving insert mode
  autocmd InsertLeave * set nopaste

  " Auto-source vimrc
  autocmd! BufWritePost .vimrc source $MYVIMRC
endif

" Use Ack for searching
if executable("ack")
  set grepprg=ack\ -H\ --nogroup\ --nocolor\ --ignore-dir=tmp\ --ignore-dir=coverage
endif
