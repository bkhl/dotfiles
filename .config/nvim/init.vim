set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc

silent! call plug#begin('~/.vim/plugged')

" Navigate and manage surrounding character pairs
Plug 'tpope/vim-surround'

" Automatically close character pairs
Plug 'Townk/vim-autoclose'

" Improved % pair matching
Plug 'vim-scripts/matchit.zip'

" Comment/uncomment
Plug 'tpope/vim-commentary'

" Show VCS changes in sign column
Plug 'mhinz/vim-signify'

" Show buffers in command bar
Plug 'bling/vim-bufferline'

" Multiple language syntax support
Plug 'sheerun/vim-polyglot'

" Linting/Bulding
Plug 'neomake/neomake'

" Ion
Plug 'vmchale/ion-vim'

" Python
Plug 'plytophogy/vim-virtualenv'

call plug#end()

" vim-signify
let g:signify_realtime = 1
let g:signify_vcs_list = [ 'git', 'bzr', 'hg', 'svn' ]

" neomake
call neomake#configure#automake('nw', 750)

" vim:set ft=vim et sw=2:
