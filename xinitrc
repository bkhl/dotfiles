#!/bin/sh

~/conf/monitor-hotplug.py

xsetroot -solid black -cursor_name left_ptr
setxkbmap -layout se -option ctrl:nocaps -option compose:menu
xset b off


eval $(keychain --noask --quiet --quick --eval id_dsa)

clipit --no-icon &
xterm -title tmux -e ~/conf/tmux/tmux.sh &
google-chrome-beta &
exec qtile --config ~/conf/Qtile/config.py
