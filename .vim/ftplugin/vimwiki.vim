" Wrap lines between words.
set linebreak

" If this is a new diary file, apply the template.
if !filereadable(expand("%")) && IsWikiDiaryFile()
    call ApplyWikiDiaryTemplate()
endif

" Set header to match filename when opening or saving file, for
" interoperability with Nextcloud Notes.
call SetWikiHeader()
autocmd BufWritePre <buffer> call SetWikiHeader()
