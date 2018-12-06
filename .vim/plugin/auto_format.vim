function! AutoFormat()
    if &ft == 'python'
        silent execute ':Isort'
        silent execute ':Black'
    endif
endfunction

autocmd BufWritePre,BufRead * call AutoFormat()
