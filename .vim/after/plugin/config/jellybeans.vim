try
    colorscheme jellybeans
catch /^Vim\%((\a\+)\)\=:E185/  
    finish
endtry

let g:jellybeans_overrides = {
\    'background': { 'ctermbg': 'none', '256ctermbg': 'none' },
\}

if has('termguicolors') && &termguicolors
    let g:jellybeans_overrides['background']['guibg'] = 'none'
endif
