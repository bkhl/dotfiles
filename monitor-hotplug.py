#!/usr/bin/env python
#
# To be triggered by a udev rule similar to this:
#     KERNEL=="card0", SUBSYSTEM=="drm", ENV{DISPLAY}=":0", \
#     ENV{XAUTHORITY}="/home/bnl/.Xauthority", \
#     RUN+="/home/bnl/conf/monitor-hotplug.py"

import re
import subprocess


####
# Sorting outputs as they should be arranged, left to right.


output_order = (
    'LVDS',
    'LVDS-1',
    'DP-0',
    'DP-1',
    'DP-2',
    'DisplayPort-0',
    'DisplayPort-1',
    'DisplayPort-2',
    'VGA-0',
    'VGA-1'
)


def output_sort(x):
    """Sorting function for outputs."""
    try:
        return output_order.index(x)
    except ValueError:
        return len(output)


####
# Run xrandr --query and parse output.

xrandr_regexp = re.compile(r"""
    (?P<SCREEN>
        ^Screen\ (?P<screen>[0-9]+):
    ) |
    (?P<OUTPUT>
        ^(?P<output>.+)\ (?P<status>(?:dis)?connected)
    ) |
    (?P<PREFERRED_MODE>
        ^\ {3}(?P<width>[0-9]+)x(?P<height>[0-9]+).*\+
    )
""", re.VERBOSE)

screens = {}
current_screen = None
current_output = None

with subprocess.Popen(['xrandr', '--query'],
                      stdout=subprocess.PIPE) as xrandr_query:
    for line in (bytes.decode(l,
                 encoding='utf-8') for l in xrandr_query.stdout.readlines()):
        match = xrandr_regexp.match(line)
        if match:
            if match.group('SCREEN'):
                screen = int(match.group('screen'))
                screens[screen] = {}
                current_screen = screens[screen]
                current_output = None
            elif match.group('OUTPUT'):
                output = match.group('output')
                current_screen[output] = {}
                current_output = current_screen[output]
                if match.group('status') == 'connected':
                    current_output['connected'] = True
                else:
                    current_output['connected'] = False
            elif match.group('PREFERRED_MODE'):
                current_output['width'] = int(match.group('width'))
                current_output['height'] = int(match.group('height'))


####
# Call xrandr with arguments according to setup.

for screen_number, outputs in screens.items():
    xrandr_call = [
        'xrandr',
        '--verbose',
        '--screen',
        str(screen_number)
    ]

    # Set output with highest vertical resolution as primary.
    primary_output = max(outputs, key=lambda o: outputs[o].get('height', 0))
    outputs[primary_output]['primary'] = True

    offset = 0
    for output in sorted(outputs, key=output_sort):
        xrandr_call.extend(['--output', output])
        if not outputs[output]['connected']:
            xrandr_call.append('--off')
        else:
            if outputs[output].get('primary', False):
                xrandr_call.append('--primary')
            xrandr_call.extend([
                '--mode',
                '{}x{}'.format(
                    outputs[output]['width'],
                    outputs[output]['height']
                ),
                '--pos',
                '{}x{}'.format(
                    offset,
                    outputs[primary_output]['height']
                    - outputs[output]['height']
                )
                ])
            offset = offset + outputs[output]['width']

    subprocess.call(xrandr_call)
