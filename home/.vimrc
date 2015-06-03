" Do not emulate vi
set nocompatible

call pathogen#infect()

runtime macros/matchit.vim

" General settings {{{
set nobackup
set nowritebackup
set noswapfile

set hidden
set autoread
set history=1000
set backspace=indent,eol,start

set wildmenu
set wildmode=longest,full
set wildignore+=tmp,.bundle,.sass-cache,.git,.svn,.hg,doc,coverage,vendor,node_modules,deps

set scrolloff=3
set splitright
set splitbelow

set t_vb=
set noerrorbells
set novisualbell
set t_Co=256
set ttyfast
set lazyredraw
set timeoutlen=500
" }}}

" Status line {{{
set laststatus=2
set showcmd
set ruler
set modelines=1
let g:airline_powerline_fonts=0
" }}}

" Colors, formatting and syntax highlighting {{{
syntax on
filetype plugin indent on
set background=dark
colorscheme smyck
set cursorline
set nowrap
set synmaxcol=200 " Do not highlight long lines
set softtabstop=2
set tabstop=2
set shiftwidth=2
set shiftround
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
" }}}

" Searching {{{
set hlsearch
set incsearch
set ignorecase
set smartcase
set showmatch

if executable("ack")
  set grepprg=ack\ -H\ --nogroup\ --nocolor\ --ignore-dir=tmp\ --ignore-dir=doc
endif
" }}}

" Folding {{{
set foldenable
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent
" }}}

" Custom key mappings {{{

" Always search with very magic mode enabled
nnoremap / /\v
vnoremap / /\v

" Do not jump over 'real' lines, only over screen lines
nnoremap j gj
nnoremap k gk

" Toggle folds with the space bar
nnoremap <space> za

" Simplify window navigation by removing the need for the W key
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" Expand %% to the file path in command mode
cnoremap %% <C-R>=expand('%:h').'/'<cr>

nnoremap <leader><leader> <c-^>
nnoremap <leader>h :set invhls<CR>
nnoremap <leader>w :set nowrap<CR>
nnoremap <leader>y "*y
vnoremap <leader>y "*ygv
nnoremap <leader>p :set paste<CR>"*]p<CR>:set nopaste<CR>
nnoremap <leader>P :set paste<CR>"*]P<CR>:set nopaste<CR>
nnoremap <leader>sc <ESC>/\v^[<=>]{7}( .*\|$)<CR>
nnoremap <leader>sh :%s/\v:(\w+) \=\>/\1:/g<cr> " Replace Ruby 1.8 Hash syntax with 1.9 Hash syntax
nnoremap <leader>gr :topleft :split config/routes.rb<cr>
nnoremap <leader>gs :topleft :split db/schema.rb<cr>
nnoremap <leader>gg :topleft :split Gemfile<cr>
nnoremap <leader>gt :topleft :split TODO<cr> " Handy for keeping a TODO list in the project root
nnoremap <leader>i mmgg=G`m
nnoremap <leader>gb :ls<cr>:b<space>
" }}}

" Plugins {{{

" Vimux {{{
nnoremap <Leader>vv :VimuxRunLastCommand<CR>
nnoremap <Leader>vp :VimuxPromptCommand<CR>
nnoremap <Leader>v[ :VimuxInspectRunner<CR>
nnoremap <Leader>vc :VimuxInterruptRunner<CR>
nnoremap <Leader>vq :VimuxCloseRunner<CR>
" }}}

" Tabular {{{
nmap <leader>t# :Tabularize /#<CR>
vmap <leader>t# :Tabularize /#<CR>
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
" }}}

" UltiSnips {{{
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
" }}}

" CtrlP {{{
" let g:ctrlp_use_caching=0
" let g:ctrlp_user_command="find %s -type f -not -path '*/.git/*' -not -path '*/.sass-cache/*' -not -path '*/tmp/*'"
let g:ctrlp_extensions = ['tag']
" }}}

" Mustache {{{
let g:mustache_abbreviations = 1
" }}}

" {{{ Projectionist
let g:rails_projections = {
  \  "app/assets/javascripts/components/*.js.cjsx": {
  \    "command": "jcomponent",
  \    "alternate": "spec/javascripts/components/%s_spec.js.coffee"
  \  },
  \
  \  "app/assets/javascripts/models/*.js.coffee": {
  \    "command": "jmodel",
  \    "alternate": "spec/javascripts/models/%s_spec.js.coffee",
  \    "template": "class @App.Models.%S extends Backbone.Model"
  \  },
  \
  \  "app/assets/javascripts/collections/*.js.coffee": {
  \    "command": "jcollection",
  \    "alternate": "spec/javascripts/collections/%s_spec.js.coffee",
  \    "template": "class @App.Collections.%S extends Backbone.Collection"
  \  },
  \
  \  "app/assets/javascripts/routers/*.coffee": {
  \    "command": "jrouter",
  \    "alternate": "spec/javascripts/routers/%s_spec.coffee",
  \    "template": "class @App.Routers.%S"
  \   }
\ }
let g:rails_gem_projections = {
  \ "active_model_serializers": {
  \   "app/serializers/*_serializer.rb": {
  \     "command": "serializer",
  \     "template": "class %SSerializer < ActiveModel::Serializer\nend",
  \     "affinity": "model",
  \     "related": "app/serializers/%s.rb",
  \     "test": ["test/serializers/%s_serializer_test.rb", "spec/serializers/%s_serializer_spec.rb"]
  \   }
  \ },
  \ "draper": {
  \   "app/decorators/*_decorator.rb": {
  \     "command": "decorator",
  \     "template": "class %SDecorator < ApplicationDecorator\nend",
  \     "affinity": "model",
  \     "related": "app/models/%s.rb",
  \     "test": ["test/decorators/%s_decorator_test.rb", "spec/decorators/%s_decorator_spec.rb"]
  \   }
  \ },
  \ "pundit": {
  \   "app/policies/*_policy.rb": {
  \     "command": "policy",
  \     "template": ["class %SPolicy < ApplicationPolicy\nend"],
  \     "affinity": "model",
  \     "related": "app/models/%s.rb",
  \     "test": ["test/policies/%s_policy_test.rb", "spec/policies/%s_policy_spec.rb"]
  \   }
  \ },
  \ "cucumber": {
  \   "features/*.feature": {
  \     "command": "feature",
  \     "alternate": "features/step_definitions/%s_steps.rb",
  \     "template": "Feature: %h\n"
  \   },
  \   "features/step_definitions/*_steps.rb": {
  \     "command": "step",
  \     "alternate": "features/%s.feature",
  \     "template": ""
  \   }
  \ }
\ }
" }}}

" {{{ Markdown
let g:markdown_fenced_languages = ['ruby', 'js=javascript', 'javascript', 'coffee', 'eruby']
" }}}

" Autocommands {{{

function! StripTrailingWhitespace()
  silent exe "normal ma<CR>"
  let saved_search = @/
  %s/\s\+$//e
  silent exe "normal `a<CR>"
  let @/ = saved_search
endfunction

if has('autocmd')

  augroup configroup

    " Remove all previously defined autocommands so re-loading this file doesn't
    " re-define everything.
    autocmd!

    " General hooks {{{

    " Automatically strip trailing whitespace from most regular source code file
    " types using a custom function.
    autocmd BufWritePre,FileWritePre *.rake,*.html,*.haml,*.rb,*.php,*.xml,*.erb,*.yml,*.scss,*.css,*.sass,*.js,*.coffee,*.cjsx,"*.ex",*.exs call StripTrailingWhitespace()

    " Resize all windows to optimum distribution whenever Vim itself (the
    " terminal window it lives in) is resized.
    autocmd VimResized * wincmd =

    " Disable paste mode when leaving insert mode.
    autocmd InsertLeave * set nopaste

    " When changing this file, always immediately reload it.
    autocmd BufWritePost .vimrc source $MYVIMRC

    " }}}

    " Ruby {{{

    " Silly Rubyists with their silly extensions. Treat the whole lot of 'em as
    " Ruby files.
    autocmd BufNewFile,BufRead {Gemfile,Guardfile,Capfile,Rakefile,Thorfile,config.ru,Vagrantfile,*.prawn} set filetype=ruby
    autocmd BufNewFile,BufRead Gemfile.lock,Procfile set filetype=yaml
    autocmd BufNewFile,BufRead *_spec.rb set filetype=rspec.ruby
    autocmd BufNewFile,BufRead *_steps.rb set filetype=rspec.ruby

    " Run Ruby files either directly or in a tmux split pane using Vimux.
    autocmd Filetype ruby nnoremap <buffer> <Leader>r :!ruby %<CR>
    autocmd Filetype ruby nnoremap <buffer> <Leader>vr :call VimuxRunCommand("clear; ruby " . bufname("%"))<CR>
    autocmd FileType ruby setlocal path+=lib/**
    autocmd FileType ruby setlocal includeexpr=substitute(substitute(substitute(v:fname,'::','/','g'),'$','.rb',''),'\\(\\<\\u\\l\\+\\\\|\\l\\+\\)\\(\\u\\)','\\l\\1_\\l\\2','g')
    autocmd FileType ruby nnoremap <buffer> <Leader>a :%!xmpfilter<cr>
    autocmd FileType ruby nnoremap <buffer> <Leader>A :normal A # =><cr>

    " Set up custom Ruby surround mapping to wrap a motion in an expect call or
    " block.
    autocmd FileType ruby let g:surround_{char2nr("x")} = "expect(\r).to"
    autocmd FileType ruby let g:surround_{char2nr("X")} = "expect { \r }.to"

    autocmd Filetype ruby command! -buffer -nargs=* Rubocop call Rubocop(<q-args>)

    " }}}

    " Javascript {{{

    " Run javascript files with Node either directly or in a split tmux pane
    " using Vimux.
    autocmd Filetype javascript nnoremap <buffer> <Leader>r :!node %<CR>
    autocmd Filetype javascript nnoremap <buffer> <Leader>k :!teaspoon %<cr>
    autocmd Filetype javascript nnoremap <buffer> <Leader>vr :call VimuxRunCommand("clear; node " . bufname("%"))<CR>
    autocmd Filetype coffee nnoremap <buffer> <Leader>r :CoffeeRun<CR>
    autocmd Filetype coffee nnoremap <buffer> <Leader>b :CoffeeCompile<CR>
    autocmd Filetype coffee nnoremap <buffer> <Leader>k :!teaspoon %<cr>
    autocmd Filetype coffee nnoremap <buffer> <Leader>vk :call VimuxRunCommand("clear; teaspoon " . bufname("%"))<CR>

    " Treat .cjsx file as coffee script
    autocmd BufNewFile,BufRead *.cjsx setlocal filetype=coffee

    " Treat JSON file as javascript
    autocmd BufNewFile,BufRead *.json setlocal filetype=javascript

    " Be smart about JSON formatting
    autocmd FileType json setlocal equalprg=python\ -m\ json.tool

    " }}}

    " Plain text, Markdown and markup {{{

    " For text and markup files do not show special characters and hard-wrap
    " text. Limit markdown and text files at 80 characters wide.
    autocmd FileType text,markdown,html,xhtml,eruby setlocal wrap linebreak nolist
    autocmd Filetype text,markdown setlocal textwidth=80

    " }}}

    " Elixir {{{

    " Treat mix.lock file as Elixir code
    autocmd BufNewFile,BufRead mix.lock setlocal filetype=elixir
    autocmd Filetype elixir nnoremap <buffer> <Leader>r :!elixir %<cr>
    autocmd Filetype elixir nnoremap <buffer> <Leader>k :!mix test %<cr>
    autocmd Filetype elixir nnoremap <buffer> <Leader>K :execute "!mix test --only line:" . line(".") . " %"<cr>
    autocmd Filetype elixir nnoremap <buffer> <Leader>vr :call VimuxRunCommand("clear; elixir " . bufname("%"))<cr>
    autocmd Filetype elixir nnoremap <buffer> <Leader>vk :call VimuxRunCommand("clear; mix test " . bufname("%"))<cr>
    autocmd Filetype elixir nnoremap <buffer> <Leader>vK :call VimuxRunCommand("clear; mix test --only line:" . line(".") . " " . bufname("%"))<cr>

    " }}}

    " Other languages {{{

    autocmd Filetype python nnoremap <buffer> <Leader>r :!python %<cr>
    autocmd Filetype sh     nnoremap <buffer> <Leader>r :!%<cr>

    " Add Railsy file extensions to the suffixes list to enable find and gf to
    " jump to Sass stylesheets.
    autocmd FileType scss setlocal suffixesadd+=.css.scss
    autocmd BufNewFile,BufRead .tmux.conf*,tmux.conf* set filetype=tmux

    autocmd BufNewFile,BufRead *.adoc set filetype=asciidoc

    " }}}

  augroup END

endif

" }}}


function! Rubocop(arg)
  let oldmakeprg = &l:makeprg
  try
    execute "set makeprg=rubocop\\ " . a:arg . "\\ %" 
    make
  finally
    let &l:makeprg = oldmakeprg
  endtry
endfunction


" vim:foldmethod=marker:foldlevel=0
