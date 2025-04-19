MAKEFLAGS += --always-make

MONOSPACE_FONT := Iosevka BKHL Sans Fixed 13

INPUT_SOURCES := [('xkb', 'us+altgr-intl'), ('xkb', 'th')]

all: config

config:
	# Fonts
	dconf write /org/gnome/desktop/interface/monospace-font-name "'$(MONOSPACE_FONT)'"

	# Disable activity hot corner
	dconf write /org/gnome/desktop/interface/enable-hot-corners 'false'

	# Add some menu bar buttons
	dconf write /org/gnome/desktop/wm/preferences/button-layout "'appmenu:minimize,maximize,close'"

	# Keyboard settings
	dconf write /org/gnome/desktop/input-sources/sources "$(INPUT_SOURCES)"
	dconf write /org/gnome/desktop/input-sources/mru-sources "$(INPUT_SOURCES)"
	dconf write /org/gnome/desktop/input-sources/per-window 'false'
	dconf write /org/gnome/desktop/input-sources/xkb-options "['lv3:ralt_switch', 'compose:caps']"
