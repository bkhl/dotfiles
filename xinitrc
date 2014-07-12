#!/bin/sh

xsetroot -solid black -cursor_name left_ptr
setxkbmap -layout se -option ctrl:nocaps
xset b off

eval $(keychain --noask --quiet --quick --eval id_dsa)

# Tray
dropboxd &
parcellite &

xterm &
exec qtile --config ~/Dropbox/Config/Qtile/config.py
