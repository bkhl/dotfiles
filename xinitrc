#!/bin/sh

~/conf/monitor-hotplug.py

xsetroot -solid black -cursor_name left_ptr
setxkbmap -layout se -option ctrl:nocaps -option compose:menu
xset b off

eval $(keychain --noask --quiet --quick --eval id_dsa)

xss-lock -- i3lock --color 161616 --ignore-empty-password &
clipit --no-icon &
xterm -e tmux attach &
google-chrome-beta &
exec i3
