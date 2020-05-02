if &runtimepath !~? "vimwiki"
  finish
endif

" My main wiki.
let g:vimwiki_list = [{
            \"path": "~/Documents/Notes/",
            \"syntax": "markdown", "ext": ".md"
            \}]

" Disable creation of temporary wikis.
let g:vimwiki_global_ext = 0

" Function to set header of current file to match filename.
"
" This helps interoparability with Nextcloud Notes, which assumes that the
" first line of the file matches the filename.
function SetWikiHeader()
    let first_line = getline(1)
    let filename = expand("%:t:r")

    if "# " . filename == first_line
        " First line is already correct.
        :
    elseif first_line =~ '^#*\s*\V' . escape(filename, '\') . '\m\s*$'
        " First line matches filename but has wrong header format. Reformat to
        " have proper format.
        call setline(1, "# " . filename)
    else
        " First line not matching filename at all. Prepend lines with filename
        " as a header.
        if first_line != ""
            call append(0, "")
        endif
        call append(0, "# " . filename)
    endif
endfunction
