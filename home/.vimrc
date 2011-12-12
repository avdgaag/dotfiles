" Do not emulate vi
set nocompatible

" Set up pathogen to load plugins
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" We don't use em
set modelines=0

" Always work with utf-8
set encoding=utf-8

" Highlight the current line
set cursorline

" Deal witg multiple buffers better
set hidden

" Automatically update changes to a file when receiving focus
set autoread

" Keep a longer history
set history=100

" Make file/command completion useful
set wildmenu
set wildmode=list:longest

" Fast editing and updating of the .vimrc (reloads on save)
map <Leader>E :e! ~/.vimrc<cr>
autocmd! bufwritepost vimrc source ~/.vimrc

" Toggle paste mode to reduce paste indent suckage
set pastetoggle=<F2>

" Quickly re-select pasted text
nnoremap <leader>v V`]

" Open related file in a vertical split window
map <Leader>f :vertical wincmd f<CR>

" Command-t buffer
nnoremap <silent> <Leader>T :CommandTBuffer<CR>

set wildignore+=tmp,.bundle,.sass-cache,.git,.svn,.hg

" Syntax highlighting
syntax on
set showmatch " matching braces
set background=dark
colorscheme Tomorrow-Night

if has("gui_running")
  " No more toolbar
  set guioptions=egmrt

  " Use light color scheme
  " set background=light
  " colorscheme mac_classic

  " Use a nice font
  set gfn=Menlo\ Regular:h13

  " Use the entire screen when in full-screen mode
  set fuopt=maxvert,maxhorz

  set list listchars=tab:\ \ ,trail:·
endif

" Intuitive Backspace
set bs=indent,eol,start

" Filetype detection
filetype plugin indent on

" Line wrapping
set nowrap
set linebreak

" ...but make sure the editor uses editor lines, not real lines
" when navigating
nnoremap j gj
nnoremap k gk

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Re-map arrow keys to do something useful, but not
" the usual navigation (force use of hjkl)
"
" The left and right keys always indent and outdent
" The up and down keys move a line up and down in normal mode
nmap <silent> <Left> <<
nmap <silent> <Right> >>
vmap <silent> <Left> <
vmap <silent> <Right> >
imap <silent> <Left> <C-D>
imap <silent> <Right> <C-T>
nmap <Up> ddkP
nmap <Down> ddp
nmap <Down> ddp

" Quickly edit files from the same directory as the current file
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>e :edit %%
map <leader>] :cnext<cr>
map <leader>[ :cprev<cr>

" Some Rails-specific jump-to-file bindings
map <leader>gr :topleft :split config/routes.rb<cr>
map <leader>gg :topleft :split Gemfile<cr>
map <leader>gn :topleft :split config/locales/nl.yml<cr>
map <leader>ge :topleft :split config/locales/en.yml<cr>
" Hany for keeping a TODO list in the project root
map <leader>gt :topleft :split TODO<cr>

" Quickly jump between current and last file
nnoremap <leader><leader> <c-^>

" Auto-reading on file changes
set autoread

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

" Set terminal title
set title

" Hide search highlighting
map <Leader>h :set invhls <CR>

" Quickly toggle NERDTree
nmap <Leader>n :NERDTreeToggle<CR>

" Reveal a file in the NERDTree
nmap <Leader>N :NERDTreeFind<CR>

" Toggle word wrap with \w
map <Leader>w :set nowrap!<CR>

" Align on hash rockets and equals
vmap <Leader>a :Align =><CR>
vmap <Leader>A :Align =<CR>

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

" Autocommands
if has('autocmd')

    " Remove trailing whitespace from various files
    autocmd BufWritePre,FileWritePre *.html,*.rb,*.php,*.xml,*.erb call StripTrailingWhitespace()

    " Use 2 spaces for tabs in ruby and associated langs
    autocmd Filetype coffee,ruby,yaml,rake,rb,ru setlocal ts=2 sw=2 expandtab
    autocmd BufNewFile,BufRead {Gemfile,Guardfile,Capfile,Rakefile,Thorfile,config.ru} set ft=ruby
    autocmd BufNewFile,BufRead Gemfile.lock set ft=yaml
    autocmd BufNewFile,BufRead *.json set ft=javascript
    autocmd BufNewFile,BufRead *.prawn set ft=ruby

    " Run ruby files using \r
    autocmd Filetype ruby,rb nmap <Leader>r :!ruby %<CR>
    autocmd Filetype ru nmap <leader>r :!rackup<CR>
    autocmd BufNewFile,BufRead *_spec.rb nmap <Leader>r :!bundle exec rspec %<CR>
    autocmd BufNewFile,BufRead *_spec.rb nmap <Leader>R :exe "!bundle exec rspec %:" . line(".")<cr>

    " Open HTML files in Safari using \r
    autocmd Filetype html nmap <Leader>r :!open -a Safari "%"<CR>

    " Set up some build commands for Coffeescript. Compile the entire
    " file or a selection with \b and always compile the file
    " on save.
    autocmd Filetype coffee nmap <Leader>b :CoffeeCompile<CR>
    autocmd Filetype coffee vmap <Leader>b :CoffeeCompile<CR>
    " autocmd BufWritePost,FileWritePost *.coffee :silent !coffee -c <afile>

    " Enable Less syntax
    au BufRead,BufNewFile *.less set filetype=less

    " Enable SCSS syntax
    au BufRead,BufNewFile *.scss set filetype=scss

    " Enable soft-wrapping for text files
    autocmd FileType text,markdown,html,xhtml,eruby setlocal wrap linebreak nolist

    " Auto-resise windows when resizing
    autocmd VimResized * wincmd =
endif

" Always open new windows below/to the right
set sb
set spr

" Use Ack for searching
if executable("ack")
    set grepprg=ack\ -H\ --nogroup\ --nocolor\ --ignore-dir=tmp\ --ignore-dir=coverage
endif

