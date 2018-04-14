""""
" Keyboard map leader

let mapleader = ","


""""
" Plugin configuration

silent! call plug#begin('~/.vim/plugged')

" Run shell commands asynchronously
Plug 'skywind3000/asyncrun.vim'

" Show buffers in command bar
Plug 'bling/vim-bufferline'

" Navigate and manage surrounding character pairs
Plug 'tpope/vim-surround'

" Git integration
if executable('git')
    Plug 'tpope/vim-fugitive'
endif

" Show VCS changes in sign column
if v:version > 704 || (v:version == 704 && has('patch1967'))
    Plug 'mhinz/vim-signify'
    let g:signify_realtime = 1
    let g:signify_vcs_list = [ 'git', 'bzr', 'hg', 'svn' ]
endif

" Automatically close character pairs
Plug 'Townk/vim-autoclose'

" Comment/uncomment
Plug 'tpope/vim-commentary'

" Language syntax support
Plug 'sheerun/vim-polyglot'

" Colour scheme
Plug 'chriskempson/base16-vim'

" Code completion
if v:version > 704 || (v:version == 704 && has('patch1578'))
    Plug 'Valloric/YouCompleteMe'
    let g:AutoClosePumvisible = {"ENTER": "", "ESC": ""}
    nnoremap <silent> gd :YcmCompleter GoTo<CR>
endif

" Linting
if v:version >= 800
    Plug 'w0rp/ale'

    let g:ale_fixers = {
    \   'haskell': [
    \       'hfmt',
    \   ],
    \   'python': [
    \       'isort',
    \       'yapf',
    \   ],
    \   'rust': [
    \       'rustfmt',
    \   ],
    \}

    let g:ale_rust_rustfmt_executable = 'rustup'
    let g:ale_rust_rustfmt_options = 'run nightly rustfmt'

    nmap <silent> <leader>lf <Plug>(ale_first)
    nmap <silent> <leader>ln <Plug>(ale_next_wrap)
    nmap <silent> <leader>lp <Plug>(ale_previous_wrap)
    nmap <silent> <leader>lo <Plug>(ale_fix)
else
    Plug 'scrooloose/syntastic'
endif

" Python
Plug 'jmcantrell/vim-virtualenv'

call plug#end()


""""
" Appearance

" Colour scheme
set background=dark
let terminal_env = $TERM
if terminal_env =~ '^xterm\(\-256color\)\=$'
    set t_Co=256
endif
if has("gui_running") || (&t_Co == 256)
    let base16colorspace=256
    colorscheme base16-bright
endif
redraw

" Syntax highlighting
syntax on

" Disable menu bar
set guioptions-=m

" Disable scroll bar
set guioptions-=r

" Disable toolbar
set guioptions-=T

" Set GUI font
set guifont=Monospace\ 11

" Hidden characters
set list
set listchars=trail:·,tab:→-

" Line numbering
set number
if exists('+relativenumber')
    set relativenumber
endif

" Status line
set laststatus=2
set statusline=%n\ %f\ %h%w%m%r\ %=%(%l,%c%V\ %=\ %L%)"""


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
set autoindent
set shiftwidth=4
set tabstop=4
set expandtab

" Persistent undo
if has("persistent_undo")
    set undodir=~/.vimundo/
    set undofile
endif

" Ignore comment leader when joining lines
if v:version > 703 || v:version == 703 && has('patch541')
  set formatoptions+=j
endif


""""
" Custom commands

" Save file as root
command! W :execute ':silent w !sudo tee % > /dev/null' | :edit!


""""
" Key bindings

" Open quickfix window
nnoremap <silent> <leader>q :copen<CR>

" Show buffers
nnoremap <silent> <leader>b :<C-u>buffers<CR>


""""
" File type specific settings

" Makefile
autocmd FileType make setlocal noexpandtab

" JSON
autocmd FileType json setlocal shiftwidth=2 tabstop=2
