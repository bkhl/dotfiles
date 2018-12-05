if !empty(glob("~/.vim/pack/plugins/start/onedark/"))
    if (has("autocmd") && !has("gui_running"))
        augroup colorset
            autocmd!
            let s:white = { "gui": "#FFFFFF", "cterm": "231", "cterm16" : "15" }
            autocmd ColorScheme * call onedark#set_highlight("Normal", { "fg": s:white })
        augroup END
    end

    colorscheme onedark
endif
