function! AutoFormat()
    if &ft == 'python'
        silent execute '%!isort -'
        execute ':Black'
    endif
endfunction

autocmd BufWritePre,BufRead * call AutoFormat()
