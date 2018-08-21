#!/bin/sh

xrandr --addmode DP-2-2 "1680x1050"
xrandr \
    --output DP-2-3 --mode 1680x1050 --pos 0x0 --rotate normal \
    --output DP-2-2 --primary --mode 1680x1050 --pos 1680x0 --rotate normal \
    --output eDP-1 --mode 1366x768 --pos 3360x0 --rotate normal \
