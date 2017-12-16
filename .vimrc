" Keyboard map leader
let mapleader = ","

" BEGIN Vundle preliminary configuration
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" BEGIN Vundle preliminary configuration

" Undo tree navigator
Plugin 'mbbill/undotree'
nnoremap <silent> <A-u> :UndotreeToggle<CR>

" Show buffers in command bar
Plugin 'bling/vim-bufferline'

" Navigate and manage surrounding character pairs
Plugin 'tpope/vim-surround'

" Git wrapper
if executable('git')
    Plugin 'tpope/vim-fugitive'
endif

" Show VCS changes in sign column
if v:version > 704 || (v:version == 704 && has('patch1967'))
    Plugin 'mhinz/vim-signify'
    let g:signify_realtime = 1
    let g:signify_vcs_list = [ 'git', 'bzr', 'hg', 'svn' ]
endif

" Automatically close character pairs
Plugin 'Townk/vim-autoclose'

" Comment/uncomment
Plugin 'tpope/vim-commentary'

" Language syntax support
Plugin 'sheerun/vim-polyglot'

" Syntax error highlighters
if v:version >= 800
    Plugin 'w0rp/ale'
    let g:ale_fixers = {
    \   'python': [
    \       'isort',
    \       'yapf',
    \   ],
    \}
    nnoremap <silent> <A-f> :ALEFirst<CR>
    nnoremap <silent> <A-n> :ALENext<CR>
    nnoremap <silent> <leader>o :ALEFix<CR>
else
    Plugin 'scrooloose/syntastic'
endif

" BEGIN Vundle final configuration
call vundle#end()
filetype plugin indent on
" END Vundle final configuration

" Show buffers
nnoremap <silent> <A-b> :<C-u>buffers<CR>

" Save file as root
command! W :execute ':silent w !sudo tee % > /dev/null' | :edit!

" Syntax highlighting
syntax on

" Colour scheme
let terminal_env = $TERM
if terminal_env =~ '^xterm\(-256color\)$'
    set t_Co=256
endif
if has("gui_running")
  colorscheme koehler
else
  colorscheme default
  set background=dark
endif

" Disable menu bar
set guioptions-=m

" Disable scroll bar
set guioptions-=r

" Disable toolbar
set guioptions-=T

" Set GUI font
set guifont=Hack\ 10

" Backup and swap file directories
set backupdir=~/.vimtmp,.
set directory=~/.vimtmp,.

" Hide buffer when abandonded
set hidden

" Hidden characters
set list
set listchars=trail:·,tab:→-

" Line numbering
set number
if exists('+relativenumber')
    set relativenumber
endif

" Allow setting Vim options in modelines
set modeline

" Indentation settings
set autoindent
set shiftwidth=4
set tabstop=4
set expandtab

" Status line
set laststatus=2
set statusline=%n\ %f\ %h%w%m%r\ %=%(%l,%c%V\ %=\ %L%)"""

" Persistent undo
if has("persistent_undo")
    set undodir=~/.vimundo/
    set undofile
endif

" Makefile preferences
autocmd FileType make setlocal noexpandtab

" JSON preferences
autocmd FileType json setlocal shiftwidth=2 tabstop=2
