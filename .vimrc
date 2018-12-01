""""
" Appearance

" Colour scheme
set background=dark

" Line numbering
set number
if exists('+relativenumber')
    set relativenumber
endif

" Status line
set laststatus=2
set statusline=%n\ %f\ %y\ %([%{&fileformat}\ %{&fileencoding?&fileencoding:&encoding}]%)\ %h%w%m%r%=%l,%c%V\ %L

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

" Map leader
let mapleader = " "

" Backup and swap file directories
set backupdir=~/.vimtmp,.
set directory=~/.vimtmp,.

" Hide buffer when abandonded
set hidden

" Allow setting Vim options in modelines
set modeline

" Indentation settings
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

" QuickFix
nnoremap <silent> <leader>f :lfirst<CR>
nnoremap <silent> <leader>n :lnext<CR>
nnoremap <silent> <leader>p :lprevious<CR>


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
