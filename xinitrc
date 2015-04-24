#!/bin/sh

~/conf/monitor-hotplug.py

xsetroot -solid black -cursor_name left_ptr
setxkbmap -layout se -option ctrl:nocaps -option compose:menu
xset b off
xrdb ~/.Xresources

eval $(keychain --noask --quiet --quick --agents ssh --eval id_dsa)

eval $(dbus-launch)
export DBUS_SESSION_BUS_ADDRESS DBUS_SESSION_BUS_PID

xss-lock -- i3lock --color 899599 --ignore-empty-password &
clipit &
nm-applet &
pa-applet &
xterm &
google-chrome-stable &
exec i3
