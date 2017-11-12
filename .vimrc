""""
" Plugins

""
" Vundle preliminary configuration

set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

""
" Utilities

Plugin 'skywind3000/asyncrun.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'bling/vim-bufferline'
Plugin 'tpope/vim-surround'

if executable('fzf')
    Plugin 'junegunn/fzf'
    Plugin 'junegunn/fzf.vim'
endif

if executable('git')
    Plugin 'tpope/vim-fugitive'
    Plugin 'airblade/vim-gitgutter'
endif

""
" Programming

Plugin 'Townk/vim-autoclose'
Plugin 'tpope/vim-commentary'
Plugin 'majutsushi/tagbar'
Plugin 'janko-m/vim-test'
Plugin 'sheerun/vim-polyglot'
if v:version > 704 || (v:version == 704 && has('patchi578'))
    Plugin 'Valloric/YouCompleteMe'
endif
if v:version >= 800
    Plugin 'w0rp/ale'
    let g:ale_fixers = {
    \   'python': [
    \       'isort',
    \       'yapf',
    \   ],
    \}
else
    Plugin 'scrooloose/syntastic'
endif

" Python
Plugin 'jmcantrell/vim-virtualenv'


""
" Vundle final configuration

call vundle#end()
filetype plugin indent on


"""
" Keyboard mapping
"

let mapleader = ","

" Buffers
nmap <silent> <leader>b :<C-U>buffers<CR>

" Open quickfix window
nmap <silent> <leader>q :copen<CR>

" Open NERDTree
map <silent> <C-n> :NERDTreeToggle<CR>

" Open tagbar
map <silent> <C-m> :TagbarToggle<CR>

" Go to declaration
nmap <silent> gd :YcmCompleter GoTo<CR>

" Navigate ALE warnings
nmap <silent> <leader>f :ALEFirst<CR>
nmap <silent> <leader>n :ALENext<CR>

" Reformat buffer
nmap <silent> <leader>o :ALEFix<CR>

" Save file as root
command! W :execute ':silent w !sudo tee % > /dev/null' | :edit!


""""
" Syntax highlighting

set background=dark
syntax on


""""
" Testing
"

let test#strategy = 'asyncrun'

nmap <silent> <leader>t :TestNearest<CR>
nmap <silent> <leader>T :TestFile<CR>
nmap <silent> <leader>a :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>
nmap <silent> <leader>g :TestVisit<CR>


""""
" Other settings
"

" Backup and swap file directories
set backupdir=~/.vimtmp,.
set directory=~/.vimtmp,.

" Buffers
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

" Fix behaviour of ENTER/ESC while using autoclose together with YouCompleteMe
let g:AutoClosePumvisible = {"ENTER": "", "ESC": ""}

" Indentation settings
set autoindent
set shiftwidth=4
set tabstop=4
set expandtab

" Status line
set laststatus=2
set statusline=%n\ %f\ %h%w%m%r\ %=%(%l,%c%V\ %=\ %L%)"""


"""""
" File formats

" Make
autocmd FileType make setlocal noexpandtab

" JSON
autocmd FileType json setlocal shiftwidth=2 tabstop=2
