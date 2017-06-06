#!/bin/sh

export DISPLAY=':0'
export XAUTHORITY="${HOME}/.Xauthority"

if [ "$(cat /sys/class/drm/card0-DP-4/status)" = 'connected' ]; then
    xrandr --newmode "1680x1050_59.98"  146.25  1680 1784 1960 2240  1050 1053 1059 1089 -hsync +vsync
    xrandr --addmode DP-2-2 "1680x1050_59.98"
fi
