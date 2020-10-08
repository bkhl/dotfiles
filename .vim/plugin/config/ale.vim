let g:ale_completion_autoimport = v:true
let g:ale_completion_enabled =  v:true
let g:ale_lint_on_save = v:true
let g:ale_lint_on_text_changed = v:true

nmap <silent> <leader>i <Plug>(ale_import)
nmap <silent> <leader>gd <Plug>(ale_go_to_definition)
