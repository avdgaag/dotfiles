" Do not emulate vi
set nocompatible

" Force 256 colors
set t_Co=256

" Set up pathogen to load plugins
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" Always show status line
set laststatus=2
set statusline=%n              " buffer number
set statusline+=\ %-.40F       " filename
set statusline+=\ %y           " filetype
set statusline+=%m             " modified flag
set statusline+=%r             " read only flag
set statusline+=%q             " quick fix
set statusline+=%=             " left/right separator
set statusline+=%{v:register}\ " active register
set statusline+=%c,            " cursor column
set statusline+=%l/%L          " cursor line/total lines

" Use pretty characters for horizontal and vertical split bars
set fillchars=stl:-,stlnc:-,vert:│

" We don't use em
set modelines=0

" Always work with utf-8
set encoding=utf-8

" Highlight the current line and column
nnoremap <Space> :set cursorcolumn!<Bar>set cursorline!<CR>

" Deal witg multiple buffers better
set hidden

" Automatically update changes to a file when receiving focus
set autoread

" Keep a longer history
set history=100

" Make file/command completion useful
set wildmenu
set wildmode=list:longest,list:full

" Fast editing and updating of the .vimrc (reloads on save)
map <Leader>E :e! ~/.vimrc<cr>

" Toggle paste mode to reduce paste indent suckage
set pastetoggle=<F2>

" Open related file in a vertical split window
map <Leader>f :vertical wincmd f<CR>

set wildignore+=tmp,.bundle,.sass-cache,.git,.svn,.hg,doc

" Syntax highlighting
syntax on
set showmatch " matching braces
set background=dark
colorscheme smyck

" Find merge conflict markers
nmap <silent> <leader>cf <ESC>/\v^[<=>]{7}( .*\|$)<CR>

" Command-T
nnoremap <silent> <C-b> :CommandTBuffer<CR>
nnoremap <silent> <C-t> :CommandT<CR>
let g:CommandTMaxHeight=20
let g:CommandTMatchWindowReverse=1
let g:CommandTAcceptSelectionSplitMap='<C-g>'

" Tabular
nmap <Leader>t= :Tabularize /=<CR>
vmap <Leader>t= :Tabularize /=<CR>
nmap <Leader>t: :Tabularize /:\zs /l0<CR>
vmap <Leader>t: :Tabularize /:\zs /l0<CR>
nmap <Leader>t, :Tabularize /,\zs /l0<CR>
vmap <Leader>t, :Tabularize /,\zs /l0<CR>
nmap <Leader>t> :Tabularize /=>\zs<CR>
vmap <Leader>t> :Tabularize /=>\zs<CR>
nmap <Leader>t" :Tabularize /"<CR>
vmap <Leader>t" :Tabularize /"<CR>
nmap <Leader>t{ :Tabularize /{<CR>
vmap <Leader>t{ :Tabularize /{<CR>

set list listchars=tab:\·\ ,trail:·,eol:¬

" Intuitive Backspace
set bs=indent,eol,start

" Filetype detection
filetype plugin indent on

" Line wrapping
set nowrap
set linebreak

" ...but make sure the editor uses editor lines, not real lines when
" navigating
nnoremap j gj
nnoremap k gk

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Re-map arrow keys to do something useful, but not the usual navigation
" (force use of hjkl)
"
" Cycle up and down through buffers, left and right to indent or outdent
nmap <up>   :bp<cr>
nmap <down> :bn<cr>
nmap <silent> <Left> <<
nmap <silent> <Right> >>
vmap <silent> <Left> <
vmap <silent> <Right> >
imap <silent> <Left> <C-D>
imap <silent> <Right> <C-T>

" Quickly edit files from the same directory as the current file
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>e :edit %%
map <leader>] :cnext<cr>
map <leader>[ :cprev<cr>

" Some Rails-specific jump-to-file bindings
map <leader>gr :topleft :split config/routes.rb<cr>
map <leader>gs :topleft :split db/schema.rb<cr>
map <leader>gg :topleft :split Gemfile<cr>
map <leader>gn :topleft :split config/locales/nl.yml<cr>
map <leader>ge :topleft :split config/locales/en.yml<cr>

" Replace Ruby 1.8 Hash syntax with 1.9 Hash syntax
nmap <leader>rh :%s/\v:(\w+) \=\>/\1:/g<cr>

" Handy for keeping a TODO list in the project root
map <leader>gt :topleft :split TODO<cr>

" Quickly start a big-ass top-left split
map <leader>ts :topleft :split 

" Quickly jump between current and last file
nnoremap <leader><leader> <c-^>

" Status line
set showcmd
set ruler
set nu

" Scrollbars
set sidescrolloff=2
set numberwidth=4

" Maintain more context around the cursor
set scrolloff=3

" Do not keep backup files. We use source control anyway.
set nobackup
set nowritebackup
set noswapfile

" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

" Search with sane regex syntax by default
nnoremap / /\v
vnoremap / /\v

" Extract spec local variable into Rspec let(...) { ... }
" foo = bar -> let(:foo) { bar }
let @l='jmlkdd?\vdescribe|context?p==I:^ys2w)Iletf=xys${`l'

" Set terminal title
set title

" Hide search highlighting
map <Leader>h :set invhls <CR>

" Toggle word wrap with \w
map <Leader>w :set nowrap!<CR>

" Indenting
set softtabstop=4
set tabstop=4
set shiftwidth=4
set expandtab
set ai " Automatically set the indent of a new line
set si " Smart indent

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

" Vimux
nnoremap <Leader>vv :VimuxRunLastCommand<CR>
nnoremap <Leader>vp :VimuxPromptCommand<CR>
nnoremap <Leader>v[ :VimuxInspectRunner<CR>
nnoremap <Leader>vc :VimuxInterruptRunner<CR>
nnoremap <Leader>vq :VimuxCloseRunner<CR>

" Autocommands
if has('autocmd')

    " Remove trailing whitespace from various files
    autocmd BufWritePre,FileWritePre *.html,*.rb,*.php,*.xml,*.erb,*.yml call StripTrailingWhitespace()

    " Use 2 spaces for tabs in ruby and associated langs
    autocmd Filetype coffee,ruby,yaml,rake,rb,ru setlocal ts=2 sw=2 expandtab
    autocmd BufNewFile,BufRead {Gemfile,Guardfile,Capfile,Rakefile,Thorfile,config.ru,Vagrantfile,*.prawn} set ft=ruby
    autocmd BufNewFile,BufRead Gemfile.lock,Procfile set ft=yaml
    autocmd BufNewFile,BufRead *.json set ft=javascript

    " Run ruby files using \r
    autocmd Filetype ruby,rb nnoremap <Leader>r :!ruby %<CR>
    autocmd Filetype ruby,rb nnoremap <Leader>vr :call VimuxRunCommand("clear; ruby " . bufname("%"))<CR>
    autocmd BufNewFile,BufRead *_spec.rb nnoremap <Leader>k :!bundle exec rspec %<CR>
    autocmd BufNewFile,BufRead *_spec.rb nnoremap <Leader>K :exe "!bundle exec rspec %\:" . line(".")<cr>
    autocmd BufNewFile,BufRead *_spec.rb nnoremap <Leader>vk :call VimuxRunCommand("clear; bundle exec rspec " . bufname("%"))<CR>
    autocmd BufNewFile,BufRead *_spec.rb nnoremap <Leader>vK :call VimuxRunCommand("clear; bundle exec rspec " . bufname("%") . ":" . line("."))<CR>

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

" Always open new windows below/to the right
set sb
set spr

" Use Ack for searching
if executable("ack")
    set grepprg=ack\ -H\ --nogroup\ --nocolor\ --ignore-dir=tmp\ --ignore-dir=coverage
endif
