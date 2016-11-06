"""
" Pathogen

execute pathogen#infect()

""""
" Syntax highlighting

set background=dark
syntax on


""""
" Syntax checking

let g:syntastic_check_on_open = 1
let g:syntastic_python_checkers = ['python', 'pyflakes']
let g:syntastic_quiet_messages = { "regex": '\mline too long' }


""""
" File formats

filetype plugin on

" Make
autocmd FileType make setlocal noexpandtab

" JSON
autocmd FileType json setlocal shiftwidth=2 tabstop=2


""""
" Custom commands

" Save file as root
command W :execute ':silent w !sudo tee % > /dev/null' | :edit!


""""
" Backup and swap file directories

set backupdir=~/.vimtmp,.
set directory=~/.vimtmp,.

""""
" Other

set modeline
set autoindent
set shiftwidth=4
set tabstop=4
set expandtab
