" Set header to match filename when opening or saving file, for
" interoperability with Nextcloud Notes.
call SetWikiHeader()
autocmd BufWritePre <buffer> call SetWikiHeader()
