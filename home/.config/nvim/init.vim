set number
set background=dark
set synmaxcol=120
colorscheme dracula
set ignorecase
set smartcase
set tabstop=8
set softtabstop=0
set expandtab
set shiftwidth=2
set smarttab
set laststatus=2
set showtabline=1
set guioptions-=e
set wildignore+=tmp,.bundle,.sass-cache,.git,.svn,.hg,doc,coverage,vendor,node_modules,deps,_build,elm-stuff,ios,android
set fillchars=vert:│
set listchars=tab:\·\ ,trail:·,eol:¬
set numberwidth=4
set splitright
set splitbelow
set nowrap

" Customize colouring of listchars
hi NonText ctermfg=61 ctermbg=NONE cterm=NONE guifg=#525563 guibg=NONE gui=NONE
hi SpecialKey ctermfg=61 ctermbg=NONE cterm=NONE guifg=#525563 guibg=NONE gui=NONE

" Always search with very magic mode enabled
nnoremap / /\v
vnoremap / /\v

nnoremap <leader>w :set wrap!<CR>
nnoremap <leader>i :set list!<CR>

" Do not jump over 'real' lines, only over screen lines
nnoremap j gj
nnoremap k gk

" Simplify window navigation by removing the need for the W key
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" Expand %% to the file path in command mode
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" Switch to other buffer
nnoremap <leader><leader> <c-^>

" Hide search highlights
nnoremap <leader>h :set invhls<CR>

" EasyAlign
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)


let g:tabprefix=''
let g:python3_host_prog='/usr/local/bin/python3'
let g:ackprg = 'ag --vimgrep --smart-case'
let g:ctrlp_extensions = ['tag']
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1

" Ale
let g:ale_linters = {
\   'javascript': ['eslint', 'flow'],
\}
let g:ale_fixers = {
\   'javascript': ['prettier', 'eslint'],
\}
nnoremap <Leader>f <Plug>(ale_fix)
nnoremap ]s :ALENextWrap<cr>
nnoremap [s :ALEPreviousWrap<cr>
let g:ale_fix_on_save = 1
let g:ale_javascript_prettier_options = '--trailing-comma all'

" Javascript.vim
let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_flow = 1

" RSpec.vim mappings
map <Leader>t :call RunCurrentSpecFile()<cr>
map <Leader>s :call RunNearestSpec()<cr>
map <Leader>r :call RunLastSpec()<cr>
let g:rspec_command = 'split | terminal bin/rspec {spec}'

cnoreabbrev Ag Ack

autocmd FileType elm set shiftwidth=4

" Plugins
call plug#begin('~/.config/nvim/plugged')
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-git'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-flagship'
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-rbenv'
Plug 'tpope/vim-haml'
Plug 'mileszs/ack.vim'
Plug 'elixir-lang/vim-elixir'
Plug 'ElmCast/elm-vim'
Plug 'w0rp/ale'
Plug 'pangloss/vim-javascript'
Plug 'kien/ctrlp.vim'
Plug 'slashmili/alchemist.vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'thoughtbot/vim-rspec'
Plug 'slim-template/vim-slim'
Plug 'SirVer/ultisnips'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-eunuch'
call plug#end()

" Custom function to save the cursor position, strip trailing whitespace
" and return to old position
function! StripTrailingWhitespace()
  silent exe "normal ma<CR>"
  let saved_search = @/
  %s/\s\+$//e
  silent exe "normal `a<CR>"
  let @/ = saved_search
endfunction

autocmd FileType ruby,html,haml,css,js,vim autocmd BufWritePre <buffer> :call StripTrailingWhitespace()
autocmd VimResized * wincmd =
autocmd InsertLeave * set nopaste
autocmd BufWritePost init.vim source $MYVIMRC
