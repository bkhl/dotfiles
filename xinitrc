#!/bin/sh

~/conf/monitor-hotplug.py

xsetroot -solid black -cursor_name left_ptr
setxkbmap -layout se -option ctrl:nocaps -option compose:menu
xset b off

eval $(keychain --noask --quiet --quick --agents ssh --eval id_dsa)

xss-lock -- i3lock --color 899599 --ignore-empty-password &
clipit --no-icon &
nm-applet &
xterm &
google-chrome-stable &
exec i3
