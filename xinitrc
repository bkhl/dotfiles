#!/bin/sh

xsetroot -solid black -cursor_name left_ptr
setxkbmap -layout se -option ctrl:nocaps -option compose:menu
xset b off

eval $(keychain --noask --quiet --quick --eval id_dsa)

# Tray
parcellite &

~/conf/monitor-hotplug.py
st &
exec qtile --config ~/conf/Qtile/config.py
