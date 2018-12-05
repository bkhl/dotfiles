if exists("g:loaded_neomake")
    call neomake#configure#automake('nw', 750)

    " Python
    let g:neomake_python_enabled_makers = ['python', 'flake8']
    let g:neomake_python_python_exe = 'python3'

    " Disabled for Rust (using RLS instead)
    let g:neomake.automake.ignore_filetypes += ['rust']
endif
