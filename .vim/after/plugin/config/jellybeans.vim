if &runtimepath !~? "jellybeans"
    finish
endif

let g:jellybeans_overrides = { 'background': {} }

if has('gui')
    let g:jellybeans_overrides['background']['guibg'] = '000000'
else
    let g:jellybeans_overrides['background']['ctermbg'] = 'none'
    let g:jellybeans_overrides['background']['256ctermbg'] = 'none'

    if has('termguicolors') && &termguicolors
        let g:jellybeans_overrides['background']['guibg'] = 'none'
    endif
endif

colorscheme jellybeans
