"""
" Pathogen

execute pathogen#infect()

""""
" Syntax highlighting

autocmd colorscheme * highlight ColorColumn ctermbg=8 guibg='#303030'
colorscheme torte
syntax on


""""
" Syntax checking

let g:syntastic_check_on_open = 1


""""
" File formats

" Make
autocmd FileType make setlocal noexpandtab


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
