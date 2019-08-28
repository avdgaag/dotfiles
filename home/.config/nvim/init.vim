set hidden
set number
set background=dark
set synmaxcol=200
colorscheme Tomorrow-Night
set ignorecase
set smartcase
set tabstop=8
set softtabstop=0
set expandtab
set shiftwidth=2
set smarttab
set laststatus=2
set showtabline=2
set guioptions-=e
set wildignore+=tmp,.bundle,.sass-cache,.git,.svn,.hg,doc,coverage,vendor,node_modules,deps,_build,elm-stuff,ios,android
set fillchars=vert:│
set listchars=tab:\·\ ,trail:·,eol:¬
set numberwidth=4
set splitright
set splitbelow
set nowrap
set inccommand=nosplit

" Customize colouring of listchars
" hi NonText ctermfg=61 ctermbg=NONE cterm=NONE guifg=#525563 guibg=NONE gui=NONE
" hi SpecialKey ctermfg=61 ctermbg=NONE cterm=NONE guifg=#525563 guibg=NONE gui=NONE

" Always search with very magic mode enabled
nnoremap / /\v
vnoremap / /\v

nnoremap <leader>w :set wrap!<CR>
nnoremap <leader>i :set list!<CR>

" Do not jump over 'real' lines, only over screen lines
nnoremap j gj
nnoremap k gk

" Shortcut to edit Vim configuration
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

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

" Escape from terminal mode to normal mode using Esc
tnoremap <Esc> <C-\><C-n>
tnoremap <C-v><Esc> <Esc>

" EasyAlign
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)


let g:tabprefix=''
let g:tablabel= "%N%{flagship#tabmodified()} %{flagship#tabcwds('shorten', ',')}"
let g:python3_host_prog='/usr/local/bin/python3'
let g:ackprg = 'ag --vimgrep --smart-case'
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1

" Ale
let g:ale_linters = {
\   'javascript': ['eslint', 'flow'],
\   'typescript': ['tslint'],
\   'json': ['prettier'],
\   'ruby': ['standardrb']
\}
let g:ale_fixers = {
\   'javascript': ['prettier', 'eslint'],
\   'typescript': ['prettier', 'tslint'],
\   'json': ['prettier'],
\   'ruby': ['standardrb']
\}
nnoremap <Leader>f <Plug>(ale_fix)
nmap <silent> [W <Plug>(ale_first)
nmap <silent> [w <Plug>(ale_previous)
nmap <silent> [w <Plug>(ale_next)
nmap <silent> [W <Plug>(ale_last)

let g:ale_fix_on_save = 1
let g:ale_javascript_prettier_options = '--trailing-comma none'

" Javascript.vim
let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_flow = 1

" vim-test
let test#strategy = "neovim"
let test#ruby#rspec#executable = 'bin/rspec'
nmap <silent> <leader>tn :TestNearest<CR>
nmap <silent> <leader>tf :TestFile<CR>
nmap <silent> <leader>ts :TestSuite<CR>
nmap <silent> <leader>tl :TestLast<CR>
nmap <silent> <leader>tv :TestVisit<CR>

cnoreabbrev Ag Ack

let g:elm_format_autosave = 1
autocmd FileType elm set shiftwidth=4

" Use existing vim session in terminal
if has('nvim') && executable("nvr")
  let $VISUAL = "nvr -cc split --remote-wait +'set bufhidden=wipe'"
endif

set runtimepath+=/Users/arjanvdgaag/code/vim-nanoc

" Plugins
call plug#begin('~/.config/nvim/plugged')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-git'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-flagship'
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-markdown'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-rbenv'
Plug 'tpope/vim-haml'
Plug 'tpope/vim-dispatch'
Plug 'mustache/vim-mustache-handlebars'
Plug 'radenling/vim-dispatch-neovim'
Plug 'mileszs/ack.vim'
Plug 'elixir-lang/vim-elixir'
Plug 'ElmCast/elm-vim'
Plug 'w0rp/ale'
Plug 'pangloss/vim-javascript'
Plug 'slashmili/alchemist.vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'janko-m/vim-test'
Plug 'slim-template/vim-slim'
Plug 'SirVer/ultisnips'
Plug 'junegunn/vim-easy-align'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-projectionist'
Plug 'mattn/emmet-vim'
Plug 'posva/vim-vue'
Plug 'leafgarland/typescript-vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
call plug#end()

" Use deoplete.
let g:deoplete#enable_at_startup = 1

let g:UltiSnipsSnippetDirectories=[$HOME.'/.config/nvim/ultisnips']

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

" Enter insert mode immediately in terminal buffers
autocmd TermOpen * startinsert
