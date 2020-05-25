if &runtimepath !~? "papercolor"
    finish
endif

set background=light

let g:PaperColor_Theme_Options = {
  \   'theme': {
  \     'default': {
  \       'transparent_background': 1
  \     }
  \   }
  \ }

colorscheme PaperColor
