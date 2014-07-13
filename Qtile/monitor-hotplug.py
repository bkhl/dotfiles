#!/usr/bin/env python

# To be triggered by a udev rule similar to this:
# KERNEL=="card0", SUBSYSTEM=="drm", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/bnl/.Xauthority", RUN+="/home/bnl/conf/Qtile/monitor-hotplug.py"

import subprocess

# Appears to be necessary to refresh certain /sys/class/drm/*/modes files
subprocess.check_call('xrandr', stdout=open('/dev/null', 'a'))

class Monitor:
    primary = False
    internal = False
    connected = False
    resolution = None
    position = None

    def __init__(self, device, label):
        self.device = device
        self.label = label

monitors = []
for device, label in (
    ('card0-DP-1', 'DisplayPort-0'),
    ('card0-DP-2', 'DisplayPort-1'),
    ('card0-DP-3', 'DisplayPort-2'),
    ('card0-LVDS-1', 'LVDS'),
    ('card0-VGA-1', 'VGA-0'),
):
    monitors.append(Monitor(device, label))

# Check which monitors are connected.
for monitor in monitors:
    with open('/sys/class/drm/{}/status'.format(monitor.device)) as status_file:
        if status_file.read().rstrip() == 'connected':
            monitor.connected = True

# Extract highest available resolution for connected monitors.
for monitor in monitors:
    if monitor.connected:
        with open('/sys/class/drm/{}/modes'.format(monitor.device)) as modes_file:
            monitor.resolution = tuple([int(x) for x in modes_file.readline().rstrip().split('x')])

# Set the monitor with the highest vertical resolution as primary.
primary_monitor = max([m for m in monitors if m.connected], key=lambda m: m.resolution[1])
primary_monitor.primary = True

# Set the internal monitor.
internal_monitor = next(m for m in monitors if m.label == 'LVDS')
internal_monitor.internal = True
internal_monitor.position = (0, 0)

# Calculate positions
offset = internal_monitor.resolution[0]
for monitor in [m for m in monitors if m.connected]:
    if not monitor.internal:
        monitor.position = (offset, 0)
        offset = offset + monitor.resolution[0]
    if not monitor.primary:
        monitor.position = (monitor.position[0], primary_monitor.resolution[1] - monitor.resolution[1])

xrandr_call = [ 'xrandr' ]

# Build xrandr command line.
for monitor in monitors:
    xrandr_call.extend(('--output', monitor.label))
    if not monitor.connected:
        xrandr_call.append('--off')
        continue
    if monitor.primary:
        xrandr_call.append('--primary')
    xrandr_call.extend([
        '--mode',
        '{}x{}'.format(*monitor.resolution),
        '--pos',
        '{}x{}'.format(*monitor.position)
    ])

# Call xrandr
subprocess.check_call(xrandr_call)
