INTERFACE_FONT := Inter BKHL Variable 11
DOCUMENT_FONT := Inter BKHL Variable 11
MONOSPACE_FONT := Iosevka BKHL Serif Fixed 13

INPUT_SOURCES := [('xkb', 'us+altgr-intl'), ('xkb', 'th')]

.PHONY: all config

all: config

config:
	# Fonts
	dconf write /org/gnome/desktop/interface/font-name "'$(INTERFACE_FONT)'"
	dconf write /org/gnome/desktop/interface/document-font-name "'$(DOCUMENT_FONT)'"
	dconf write /org/gnome/desktop/interface/monospace-font-name "'$(MONOSPACE_FONT)'"
	dconf write /org/gnome/desktop/wm/preferences/titlebar-font "'$(INTERFACE_FONT)'"

	# Disable activity hot corner
	dconf write /org/gnome/desktop/interface/enable-hot-corners 'false'

	# Add some menu bar buttons
	dconf write /org/gnome/desktop/wm/preferences/button-layout "'appmenu:minimize,maximize,close'"

	# Keyboard settings
	dconf write /org/gnome/desktop/input-sources/sources "$(INPUT_SOURCES)"
	dconf write /org/gnome/desktop/input-sources/mru-sources "$(INPUT_SOURCES)"
	dconf write /org/gnome/desktop/input-sources/per-window 'false'
	dconf write /org/gnome/desktop/input-sources/xkb-options "['lv3:ralt_switch', 'compose:caps']"
