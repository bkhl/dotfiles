""""
" Keyboard map leader

let mapleader = " "

""""
" Plugins

silent! call plug#begin('~/.vim/plugged')

" Show buffers in command bar
Plug 'bling/vim-bufferline'

" Navigate and manage surrounding character pairs
Plug 'tpope/vim-surround'

" Show VCS changes in sign column
if v:version > 704 || (v:version == 704 && has('patch1967'))
    Plug 'mhinz/vim-signify'
    let g:signify_realtime = 1
    let g:signify_vcs_list = [ 'git', 'bzr', 'hg', 'svn' ]
endif

" Automatically close character pairs
Plug 'Townk/vim-autoclose'

" Improved % pair matching
Plug 'vim-scripts/matchit.zip'

" Comment/uncomment
Plug 'tpope/vim-commentary'

" Language syntax support
Plug 'sheerun/vim-polyglot'

" async job control
Plug 'prabirshrestha/async.vim'

" async completion
Plug 'prabirshrestha/asyncomplete.vim'

" LSP
Plug 'prabirshrestha/vim-lsp'
Plug 'prabirshrestha/asyncomplete-lsp.vim'

" Linting
if v:version >= 800
    Plug 'w0rp/ale'

    let g:ale_fixers = {
    \   'python': [
    \       'isort',
    \       'black',
    \   ],
    \   'rust': [
    \       'rustfmt',
    \   ],
    \}

    nmap <silent> <leader>f <Plug>(ale_first)
    nmap <silent> <leader>n <Plug>(ale_next_wrap)
    nmap <silent> <leader>p <Plug>(ale_previous_wrap)
    nmap <silent> <leader>o <Plug>(ale_fix)
else
    Plug 'scrooloose/syntastic'
endif

" File system explorer
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
map <C-n> :NERDTreeToggle<CR>

" Ion
Plug 'vmchale/ion'

" Python
Plug 'plytophogy/vim-virtualenv'

" Rust
Plug 'rust-lang/rust.vim'
let g:rustfmt_autosave = 1
if executable('rls')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'rls',
        \ 'cmd': {server_info->['rustup', 'run', 'nightly', 'rls']},
        \ 'whitelist': ['rust'],
        \ })
endif

call plug#end()


""""
" Appearance

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^linux'
  set t_Co=16
endif

" Colour scheme
if has("gui_running")
    colorscheme koehler
else
    colorscheme default
endif
set background=dark

" Disable menu bar
set guioptions-=m

" Disable scroll bar
set guioptions-=r

" Disable toolbar
set guioptions-=T

" Set GUI font
set guifont=Monospace\ 11

" Line numbering
set number
if exists('+relativenumber')
    set relativenumber
endif

" Status line
set laststatus=2
set statusline=%n\ %f\ %h%w%m%r\ %=%(%l,%c%V\ %=\ %L%)"""

" Syntax highlighting
if has('syntax') && !exists('g:syntax_on')
  syntax enable
endif

" Show cursor coordinates
set ruler

" Scroll before cursor reaches top/bottom of screen
set scrolloff=5
set sidescrolloff=5

" Show as much of possible of last line
set display+=lastline

" Hidden characters
set list
set listchars=trail:-,tab:>-,extends:>,precedes:<,nbsp:+


""""
" Behaviour

" Backup and swap file directories
set backupdir=~/.vimtmp,.
set directory=~/.vimtmp,.

" Hide buffer when abandonded
set hidden

" Allow setting Vim options in modelines
set modeline

" Indentation settings
"
if has('autocmd')
  filetype plugin indent on
endif
set autoindent
set backspace=indent,eol,start
set complete-=i
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4

" Persistent undo
if has("persistent_undo")
    set undodir=~/.vimundo/
    set undofile
endif

" Disable increment/decrement for octal numbers
set nrformats-=octal

" Incremental search
set incsearch
" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

" Enhanced command-line completion
set wildmenu

" Use UTF-8 for Latin-1 files in GUI
if &encoding ==# 'latin1' && has('gui_running')
  set encoding=utf-8
endif

" Delete comment character when joining commented lines
if v:version > 703 || v:version == 703 && has("patch541")
  set formatoptions+=j
endif

" Automatically reload file if edited outside of Vim
set autoread

" Max command history
set history=1000

" Persist uppercase global variables not starting with '_'
set viminfo^=!

" Don't store global options in saved sessions
set sessionoptions-=options

" Break undo before C-u in insert mode
inoremap <C-U> <C-G>u<C-U>


""""
" Custom commands

" Save file as root
command! W :execute ':silent w !sudo tee % > /dev/null' | :edit!


""""
" File type specific settings

" Makefile
autocmd FileType make setlocal noexpandtab

" JSON
autocmd FileType json setlocal shiftwidth=2 tabstop=2


" vim:set ft=vim et sw=2:
