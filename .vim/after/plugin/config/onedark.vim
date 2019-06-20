if empty(glob("~/.vim/pack/plugins/start/onedark/"))
    finish
endif

if (has("autocmd")) " && !has("gui_running"))
    augroup colorset
        autocmd!
        let s:white = { "gui": "#ffffff", "cterm": "231", "cterm16" : "15" }
        let s:black = { "gui": "#000000", "cterm": "16", "cterm16" : "0" }
        autocmd ColorScheme * call onedark#set_highlight("Normal", { "fg": s:white, "bg": s:black })
    augroup END
end

colorscheme onedark
