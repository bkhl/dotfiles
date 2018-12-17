""""
" Appearance

" Terminal settings
if &term == "screen-256color"
    " Disable flow control.
    silent !stty -ixon

    " See :help xterm-true-color
    let &t_8f = "\<Esc>[38:2:%lu:%lu:%lum"
    let &t_8b = "\<Esc>[48:2:%lu:%lu:%lum"

    " Escape sequences for changing cursor shape.
    let &t_SI = "\<Esc>[5 q"
    let &t_SR = "\<Esc>[3 q"
    let &t_EI = "\<Esc>[1 q"

    set termguicolors
endif

" Background colour
set background=dark

" Syntax highlighting
if has('syntax') && !exists('g:syntax_on')
    syntax on
endif

" Line numbering
set number
if exists('+relativenumber')
    set relativenumber
endif

" Status line
set laststatus=2
set statusline=%n\ %f\ %y\ %([%{&fileencoding?&fileencoding:&encoding}\ %{&fileformat}]%)\ %h%w%m%r%=%l,%c%V\ %L

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
set hlsearch
" Clear the highlighting of :set hlsearch.
nmap <silent> <esc><esc> :nohlsearch<CR>

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

" Leave insert mode with C-l
inoremap <C-L> <Esc>

" QuickFix
nnoremap <silent> <leader>f :lfirst<CR>
nnoremap <silent> <leader>n :lnext<CR>
nnoremap <silent> <leader>p :lprevious<CR>

" Save file
nnoremap <silent> <leader>s :update<CR>

""""
" Custom commands

" Save file as root
command! W :execute ':silent w !sudo tee % > /dev/null' | :edit!
