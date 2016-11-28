""""
" Vundle

" Vundle preliminary configuration
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

" Vundle plugin list
Plugin 'bling/vim-bufferline'
Plugin 'chase/vim-ansible-yaml'
Plugin 'elixir-lang/vim-elixir'
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-surround'

" Vundle final configuration
call vundle#end()
filetype plugin indent on


""""
" Syntax highlighting

set background=dark
syntax on


""""
" Syntax checking with Syntastic

let g:syntastic_check_on_open = 1
let g:syntastic_python_checkers = ['python', 'pyflakes']
let g:syntastic_quiet_messages = { "regex": '\mline too long' }


""""
" File formats


" Make
autocmd FileType make setlocal noexpandtab

" JSON
autocmd FileType json setlocal shiftwidth=2 tabstop=2


""""
" Custom commands

" Save file as root
command! W :execute ':silent w !sudo tee % > /dev/null' | :edit!


""""
" Backup and swap file directories

set backupdir=~/.vimtmp,.
set directory=~/.vimtmp,.


""""
" Buffers

set hidden


""""
" Other

set modeline
set autoindent
set shiftwidth=4
set tabstop=4
set expandtab
