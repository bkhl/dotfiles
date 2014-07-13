#!/bin/sh

# To be triggered by a udev rule similar to this:
# ACTION=="add", SUBSYSTEM=="input", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/bnl/.Xauthority", RUN+="/home/bnl/conf/keyboard-hotplug.sh"

setxkbmap -layout se -option ctrl:nocaps -option compose:menu
