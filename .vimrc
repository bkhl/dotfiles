" Keyboard map leader
let mapleader = ","

" BEGIN Vundle preliminary configuration
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" BEGIN Vundle preliminary configuration

" Run shell commands asynchronously
Plugin 'skywind3000/asyncrun.vim'

" File tree navigator
Plugin 'scrooloose/nerdtree'
noremap <silent> <A-t> :NERDTreeToggle<CR>

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

" Ctags-based outline viewer
Plugin 'majutsushi/tagbar'
noremap <silent> <A-m> :TagbarToggle<CR>

" Test runner
Plugin 'janko-m/vim-test'
let test#strategy = 'asyncrun'
nnoremap <silent> <leader>t :TestNearest<CR>
nnoremap <silent> <leader>T :TestFile<CR>
nnoremap <silent> <leader>a :TestSuite<CR>
nnoremap <silent> <leader>l :TestLast<CR>
nnoremap <silent> <leader>g :TestVisit<CR>

" Language syntax support
Plugin 'sheerun/vim-polyglot'

" Colour scheme
Plugin 'chriskempson/base16-vim'

" Code completion
if v:version > 704
    Plugin 'prabirshrestha/async.vim'
    Plugin 'prabirshrestha/vim-lsp'
    Plugin 'prabirshrestha/asyncomplete.vim'
    Plugin 'prabirshrestha/asyncomplete-lsp.vim'
    if executable('pyls')
        au User lsp_setup call lsp#register_server({
            \ 'name': 'pyls',
            \ 'cmd': {server_info->['pyls']},
            \ 'whitelist': ['python'],
            \ })
    endif
    inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
    inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
    inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"
    imap <c-space> <Plug>(asyncomplete_force_refresh)
endif

" Syntax error highlighters
if v:version >= 800
    Plugin 'w0rp/ale'
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
    \   'sh': [
    \       'shfmt',
    \   ],
    \}
    nnoremap <silent> <A-f> :ALEFirst<CR>
    nnoremap <silent> <A-n> :ALENext<CR>
    nnoremap <silent> <leader>o :ALEFix<CR>
else
    Plugin 'scrooloose/syntastic'
endif

" Python
Plugin 'jmcantrell/vim-virtualenv'

" BEGIN Vundle final configuration
call vundle#end()
filetype plugin indent on
" END Vundle final configuration


" Show buffers
nnoremap <silent> <A-b> :<C-u>buffers<CR>

" Open quickfix window
nnoremap <silent> <A-q> :copen<CR>

" Save file as root
command! W :execute ':silent w !sudo tee % > /dev/null' | :edit!

" Syntax highlighting
syntax on

" Colour scheme
set background=dark
let base16colorspace=256
colorscheme base16-bright

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

" Makefile
autocmd FileType make setlocal noexpandtab

" JSON
autocmd FileType json setlocal shiftwidth=2 tabstop=2
