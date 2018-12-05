if exists("g:loaded_neomake")
    call neomake#configure#automake('nw', 750)

    let g:neomake_python_enabled_makers = ['python', 'flake8']
    let g:neomake_python_python_exe = 'python3'

    autocmd BufWritePost *.rs Neomake! cargo
endif
