if &runtimepath !~? "vimwiki"
  finish
endif

function s:GetWikiConfig(path)
    return {
                \"path": a:path,
                \"syntax": "markdown",
                \"ext": ".md",
                \"index": "Index",
                \"diary_rel_path": "Diary/",
                \"diary_index": "Diary",
                \"auto_diary_index": 1,
                \"auto_tags": 1,
                \}
endfunction

" My main wiki.
let g:vimwiki_list = [s:GetWikiConfig("~/Documents/Notes/"), s:GetWikiConfig("~/Documents/Notes/Work")]

" Disable creation of temporary wikis.
let g:vimwiki_global_ext = 0

" Function to set header of current file to match filename.
"
" This helps interoparability with Nextcloud Notes, which assumes that the
" first line of the file matches the filename.
function! SetWikiHeader()
    let l:first_line = getline(1)
    let l:filename = expand("%:t:r")

    if "# " . l:filename == l:first_line
        " First line is already correct.
        :
    elseif l:first_line =~ '^#*\s*\V' . escape(l:filename, '\') . '\m\s*$'
        " First line matches filename but has wrong header format. Reformat to
        " have proper format.
        call setline(1, "# " . l:filename)
    else
        " First line not matching filename at all. Prepend lines with filename
        " as a header.
        if l:first_line != ""
            call append(0, "")
        endif
        call append(0, "# " . l:filename)
    endif
endfunction

" Keyboard mappings

" Changed to remove conflict with Vinegar
nmap =- <Plug>VimwikiRemoveHeaderLevel
nmap =+ <Plug>VimwikiAddHeaderLevel
