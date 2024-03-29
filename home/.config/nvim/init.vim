set hidden
set number
set background=dark
set synmaxcol=200
set ignorecase
set smartcase
set tabstop=8
set softtabstop=0
set expandtab
set shiftwidth=2
set wildignore+=tmp,.bundle,.sass-cache,.git,.svn,.hg,doc,coverage,vendor,node_modules,deps,_build,elm-stuff,ios,android
set fillchars=vert:│
set splitright
set splitbelow
set inccommand=nosplit
set nobackup
set nowritebackup
set shortmess+=c
set signcolumn=yes
set autoindent
set backspace=indent,eol,start
set complete-=i
set smarttab
set nrformats-=octal
set incsearch
set ruler
set wildmenu
set scrolloff=1
set sidescrolloff=5
set display+=lastline
set clipboard+=unnamedplus
if &encoding ==# 'latin1' && has('gui_running')
  set encoding=utf-8
endif
if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif
set formatoptions+=j
if has('path_extra')
  setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif
set autoread
set history=1000
set tabpagemax=50
set sessionoptions-=options
set viewoptions-=options
if &t_Co == 8 && $TERM !~# '^Eterm'
  set t_Co=16
endif
set foldlevel=5

" == Other settings
if has('autocmd')
  filetype plugin indent on
endif
if has('syntax') && !exists('g:syntax_on')
  syntax enable
endif
let g:python3_host_prog='/Users/avdgaag/.asdf/shims/python'

" == Keybindings

nnoremap <SPACE> <Nop>
let mapleader = ' '

" Always search with very magic mode enabled
nnoremap / /\v
vnoremap / /\v

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

" Quick-find commands, help tags
nnoremap <Leader><Leader> :FzfCommands<cr>
nnoremap <Leader>h :FzfHelptags<cr>

" Project navigation
nnoremap <Leader>pf :FzfFiles<cr>
nnoremap <Leader>pb :FzfBuffers<cr>
nnoremap <Leader>pr :FzfHistory<cr>
nnoremap <Leader>pm :FzfMarks<cr>
nnoremap <Leader>pt :FzfTags<cr>
nnoremap <Leader>pT :FzfBufferTags<cr>

nnoremap <Leader>pw :FzfWindows<cr>
nnoremap <Leader>/: :FzfHistory:<cr>

command! -bang FzfRailsModels call fzf#vim#files('app/models', <bang>0)
command! -bang FzfRailsControllers call fzf#vim#files('app/controllers', <bang>0)
command! -bang FzfRailsHelpers call fzf#vim#files('app/helpers', <bang>0)
command! -bang FzfRailsJobs call fzf#vim#files('app/jobs', <bang>0)
command! -bang FzfRailsViews call fzf#vim#files('app/views', <bang>0)
command! -bang FzfRailsPolicies call fzf#vim#files('app/policies', <bang>0)
command! -bang FzfRailsMailers call fzf#vim#files('app/mailers', <bang>0)
command! -bang FzfRailsMigrations call fzf#vim#files('db/migrate', <bang>0)
command! -bang FzfRailsInitializers call fzf#vim#files('config/initializers', <bang>0)
command! -bang FzfRailsEnvironments call fzf#vim#files('config/environments', <bang>0)
command! -bang FzfRailsLib call fzf#vim#files('lib', <bang>0)

" Ruby on Rails project navigation
nnoremap <Leader>frm :FzfRailsModels<cr>
nnoremap <Leader>frc :FzfRailsControllers<cr>
nnoremap <Leader>frh :FzfRailsHelpers<cr>
nnoremap <Leader>frj :FzfRailsJobs<cr>
nnoremap <Leader>frv :FzfRailsViews<cr>
nnoremap <Leader>fro :FzfRailsPolicies<cr>
nnoremap <Leader>fr@ :FzfRailsMailers<cr>
nnoremap <Leader>frn :FzfRailsMigrations<cr>
nnoremap <Leader>fri :FzfRailsInitializers<cr>
nnoremap <Leader>fre :FzfRailsEnvironments<cr>
nnoremap <Leader>frl :FzfRailsLib<cr>

" Search
nnoremap <Leader>// :FzfRg<cr>
nnoremap <Leader>/? :FzfHistory/<cr>
nnoremap <Leader>/l :FzfLines<cr>
nnoremap <Leader>/L :FzfBLines<cr>

" Completion
imap <expr> <c-x><c-f> fzf#vim#complete#path('fd')
imap <expr> <c-x><c-k> fzf#vim#complete#word()
imap <expr> <c-x><c-l> fzf#vim#complete#line()

" Toggles
nnoremap <Leader>tw :set wrap!<cr>
nnoremap <Leader>ti :set list!<cr>
nnoremap <Leader>th :set invhls!<cr>
nnoremap <Leader>tn :set number!<cr>
nnoremap <Leader>tl :set cursorline!<cr>
nnoremap <Leader>tc :set cursorcolumn!<cr>
nnoremap <Leader>tZ :call AgToggleFolding()<cr>
nnoremap <Leader>tz za

function! AgToggleFolding()
  if &foldenable == 1
    set nofoldenable foldcolumn=0
  else
    set foldenable foldcolumn=2
  endif
endfunction

" Files
nnoremap <Leader>fd :Delete<cr>
nnoremap <Leader>fD :Unlink<cr>
nnoremap <Leader>fr :Move<cr>
nnoremap <Leader>fR :Rename<cr>
nnoremap <Leader>fc :Chmod<cr>
nnoremap <Leader>fm :Mkdir<cr>
nnoremap <Leader>fe :e ~/.config/nvim/init.vim<cr>
nnoremap <Leader>fo :silent exec "!open %"<cr>
nnoremap <Leader>fO :silent exec "!open -R %"<cr>
nmap <Leader>fj <Plug>VinegarUp
nmap <Leader>fJ <Plug>VinegarSplitUp

" LSP
nmap <Leader>gd <Plug>(coc-definition)
nmap <Leader>gy <Plug>(coc-type-definition)
nmap <Leader>gi <Plug>(coc-immplementation)
nmap <Leader>gr <Plug>(coc-references)
nmap <Leader>gR <Plug>(coc-rename)
nmap <Leader>g= :ALEFix<cr>
nmap <leader>ga <Plug>(coc-codeaction-selected)

" Tests
nnoremap <silent> <Leader>rn :TestNearest<cr>
nnoremap <silent> <Leader>rf :TestFile<cr>
nnoremap <silent> <Leader>rs :TestSuite<cr>
nnoremap <silent> <Leader>rt :TestLast<cr>
nnoremap <silent> <Leader>rv :TestVisit<cr>

" Snippets
nnoremap <Leader>is :FzfSnippets<cr>

if has('nvim')
  " quickly get out of terminal insert mode
  tnoremap <C-o> <C-\><C-n>
endif

" == Plugins

call plug#begin('~/.config/nvim/plugged')

" FZF
let g:fzf_command_prefix = 'Fzf'
let g:fzf_preview_window = ''

Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'

" Ale for linting and fixing
let g:ale_linters = {
\ 'ruby': ['rubocop'],
\ 'javascript': ['eslint'],
\ 'json': ['prettier'],
\ 'elixir': ['elixir-ls', 'credo', 'mix']
\}
let g:ale_fixers = {
\ '*': ['remove_trailing_lines', 'trim_whitespace'],
\ 'ruby': ['rubocop'],
\ 'javascript': ['prettier', 'eslint'],
\ 'json': ['prettier'],
\ 'css': ['prettier'],
\ 'sql': [{buffer -> {'command': 'sqlfmt --use-spaces --tab-width 2 --casemode lower'}}],
\ 'elixir': ['mix_format']
\}
let g:ale_elixir_elixir_ls_release = expand("/Users/arjan/code/elixir-ls/rel/")
let g:ale_fix_on_save = 1
let g:ale_linters_explicit = 1
Plug 'dense-analysis/ale'

" Testing
let test#strategy = "neovim"
Plug 'janko-m/vim-test'

" Ruby
let g:ruby_indent_block_style = 'do'
let g:ruby_indent_assignment_style = 'variable'
let g:ruby_operators = 1
let g:ruby_pseudo_operators = 1
let g:ruby_space_errors = 1
" let g:ruby_no_expensive=1
let g:ruby_fold = 1
let g:ruby_foldable_groups = 'def class module'
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-bundler'

" Snippets
let g:UltiSnipsEditSplit='context'
let g:UltiSnipsExpandTrigger='<M-/>'
Plug 'SirVer/ultisnips'

" Other plugins
Plug 'slim-template/vim-slim'
Plug 'pangloss/vim-javascript'
Plug 'leafgarland/typescript-vim'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'vim-airline/vim-airline'
Plug 'tpope/vim-commentary'
Plug 'mattn/emmet-vim'
Plug 'tpope/vim-markdown'
" Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'stsewd/fzf-checkout.vim'
Plug 'elixir-editors/vim-elixir'
Plug 'elmcast/elm-vim'
Plug 'vimwiki/vimwiki'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'lifepillar/pgsql.vim'
let g:sql_type_default = 'pgsql'
call plug#end()

" Load matchit.vim, but only if the user hasn't installed a newer version.
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

colorscheme dracula

" == Autocommands

" Auto-resize windows
autocmd VimResized * wincmd =

" Always leave paste mode
autocmd InsertLeave * set nopaste

" Auto-source init.vim when it changes
autocmd BufWritePost init.vim source $MYVIMRC

" Enter insert mode immediately in terminal buffers
autocmd TermOpen * startinsert

" == Functions

function! s:ag_checkout_do(lines)
  echo system("git checkout " . shellescape(split(a:lines[0])[0]))
endfunction

function! s:ag_checkout_branch()
  call fzf#run(fzf#wrap(
       \ 'ag_checkout_branch',
       \ {
       \   'source': 'git branch --list -vv --color=always --sort=refname:short | sed "s/^..//"',
       \   'sink*': function('s:ag_checkout_do'),
       \   'options': ['--prompt', 'Check out ', '--ansi', '--nth', '1']
       \ }
       \ ))
endfunction

nnoremap <Leader>vb :call <SID>ag_checkout_branch()<cr>
nnoremap <Leader>vl :FzfCommits<cr>
nnoremap <Leader>vL :FzfBCommits<cr>

if exists("$EXTRA_VIM")
  for path in split($EXTRA_VIM, ':')
    exec "source ".path
  endfor
endif
