function! AutoFormat()
    if &ft == 'python'
        execute ':Black'
    endif
endfunction

autocmd BufWritePre,BufRead * call AutoFormat()
