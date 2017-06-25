#!/bin/sh

export "$(gnome-keyring-daemon -s)"

xset s 600 600
xset dpms 900 900 900
xss-lock -- i3lock -n &

unclutter &

nm-applet &
parcellite &


exec i3
