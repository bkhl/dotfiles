function! AutoFormat()
    if &ft == 'python'
        execute ':Isort'
        execute ':Black'
    endif
endfunction

autocmd BufWritePre,BufRead * call AutoFormat()
