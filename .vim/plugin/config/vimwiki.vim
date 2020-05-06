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

" My wikis.
let g:vimwiki_list = [s:GetWikiConfig("~/Documents/Notes/"), s:GetWikiConfig("~/Documents/Notes/Work")]

" Disable creation of temporary wikis.
let g:vimwiki_global_ext = 0


""""
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



""""
" Create new diary entries from template

" Return true if current buffer appears to be dated dictionary page in the
" current wiki.
function! IsWikiDiaryFile()
    let l:filepath = expand("%")
    let l:diary_path = vimwiki#vars#get_wikilocal("path")
                \. vimwiki#vars#get_wikilocal("diary_rel_path")
    let l:is_in_diary_path = l:filepath =~ '^\V' . escape(l:diary_path, '\')
    let l:is_dated_file = (expand("%:t") =~
                \'^\d\{4}-\d\{2}-\d\{2}\V'
                \. escape(vimwiki#vars#get_wikilocal("ext"), '\')
                \. '\m$')
    return l:is_in_diary_path && l:is_dated_file
endfunction

" Apply diary template to current buffer.
function! ApplyWikiDiaryTemplate()
    let l:template_file = vimwiki#vars#get_wikilocal("path")
                \. vimwiki#vars#get_wikilocal("diary_rel_path")
                \. "Template"
                \. vimwiki#vars#get_wikilocal("ext")
    let l:content = readfile(l:template_file)
    call append(1, l:content[2:-1])
endfunction

""""
" Keyboard mappings

" Changed to remove conflict with Vinegar
nmap =- <Plug>VimwikiRemoveHeaderLevel
nmap =+ <Plug>VimwikiAddHeaderLevel
