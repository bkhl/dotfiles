if !exists('g:lsp_loaded')
    finish
endif

let g:lsp_signs_enabled = 1
let g:lsp_diagnostics_echo_cursor = 1

call lsp#register_server({
    \ 'name': 'rls',
    \ 'cmd': {server_info->['rls']},
    \ 'root_uri':{server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'Cargo.toml'))},
    \ 'whitelist': ['rust'],
    \ })
