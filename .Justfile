_default:
    just --list

INTERFACE_FONT := "Noto Sans 11"
DOCUMENT_FONT := "Noto Serif 11"
MONOSPACE_FONT := "Noto Sans Mono 13"

# Apply desktop configuration
configure:
    dconf write /org/gnome/desktop/interface/font-name "'{{ INTERFACE_FONT }}'"
    dconf write /org/gnome/desktop/interface/document-font-name "'{{ DOCUMENT_FONT }}'"
    dconf write /org/gnome/desktop/interface/monospace-font-name "'{{ MONOSPACE_FONT }}'"
    dconf write /org/gnome/desktop/wm/preferences/titlebar-font "'{{ INTERFACE_FONT }}'"
