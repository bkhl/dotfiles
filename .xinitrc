#!/bin/sh

export "$(gnome-keyring-daemon -s)"

nm-applet &
parcellite &

exec i3
