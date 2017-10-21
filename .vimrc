""""
" Vundle

" Vundle preliminary configuration
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

" Bufferline - Show list of buffers in command line
Plugin 'bling/vim-bufferline'

" Mappings for handling surrounding pairs
Plugin 'tpope/vim-surround'

if v:version >= 800
    " Syntastic syntax checker
    Plugin 'w0rp/ale'
else
    " ALE syntax checker
    Plugin 'scrooloose/syntastic'
endif


" Filetype support plugins

" Ansible YAML
Plugin 'chase/vim-ansible-yaml'

" Elixir
Plugin 'elixir-lang/vim-elixir'

" Markdown
Plugin 'suan/vim-instant-markdown'


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


"""
" Syntax checking with ALE
"

let g:ale_python_flake8_executable = 'python'
let g:ale_python_flake8_options = '-m flake8'


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
" Hidden characters

set list
set listchars=trail:·,tab:→-


""""
" Line numbering
"

set number
if exists('+relativenumber')
    set relativenumber
endif


""""
" Other

set modeline
set autoindent
set shiftwidth=4
set tabstop=4
set expandtab
