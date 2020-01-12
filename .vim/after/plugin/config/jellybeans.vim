if &runtimepath !~? "jellybeans"
    finish
endif

let g:jellybeans_overrides = {
\    'background': { 'ctermbg': 'none', '256ctermbg': 'none', 'guibg': '2d2d2d'},
\}

colorscheme jellybeans
