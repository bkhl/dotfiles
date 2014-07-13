#!/usr/bin/env python

# To be triggered by a udev rule similar to this:
# KERNEL=="card0", SUBSYSTEM=="drm", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/bnl/.Xauthority", RUN+="/home/bnl/conf/Qtile/monitor-hotplug.py"

import subprocess

# Appears to be necessary to refresh certain /sys/class/drm/*/modes files
subprocess.check_call('xrandr', stdout=open('/dev/null', 'a'))

monitors = {
    'card0-DP-1': { 'name': 'DisplayPort-0' },
    'card0-DP-2': { 'name': 'DisplayPort-1' },
    'card0-DP-3': { 'name': 'DisplayPort-2' },
    'card0-LVDS-1': { 'name': 'LVDS' },
    'card0-VGA-1': { 'name': 'VGA-0' },
}

for monitor in list(monitors.keys()):
    with open('/sys/class/drm/{}/status'.format(monitor)) as status_file:
        if status_file.read().rstrip() == 'disconnected':
            del(monitors[monitor])

if len(monitors) == 1:
    subprocess.check_call([
        'xrandr',
        '--output',
        'LVDS',
        '--primary',
        '--mode',
        '1600x900',
        '--pos',
        '0x0'
     ])
elif len(monitors) == 2:
    for monitor in monitors:
        with open('/sys/class/drm/{}/modes'.format(monitor)) as modes_file:
            monitors[monitor]['resolution'] = [ int(x) for x in modes_file.readline().rstrip().split('x') ]

    (primary, secondary) = sorted(monitors.keys(), key=lambda monitor: monitors[monitor]['resolution'][1], reverse=True)

    subprocess.check_call([
        'xrandr',
        '--output',
        monitors[primary]['name'],
        '--primary',
        '--mode',
        '{}x{}'.format(*monitors[primary]['resolution']),
        '--pos',
        '{}x{}'.format(monitors[secondary]['resolution'][0], 0),
        '--output',
        monitors[secondary]['name'],
        '--mode',
        '{}x{}'.format(*monitors[secondary]['resolution']),
        '--pos',
        '{}x{}'.format(0, monitors[primary]['resolution'][1] - monitors[secondary]['resolution'][1])
    ])
