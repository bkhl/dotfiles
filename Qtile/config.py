from libqtile.config import Key, Screen, Group, Match
from libqtile.command import lazy
from libqtile.dgroups import simple_key_binder
from libqtile import layout, hook, bar, widget

import netctl

BAR_TEXT = "#FFFFFF"
HIGHLIGHT = '#4169E1'
SECONDARY_HIGHLIGHT = '#806925'
BACKGROUND = '#303030'
OTHER_SCREEN = '#707070'
URGENT = '#FF0000'
URGENT_TEXT = '#FFFFFF'


####
# Key bindings

dmenu_options = "-i -fn 'Envy Code R-11'"

mod = 'mod4'

keys = [
    # Switch between windows in current stack pane
    Key([mod], 'k', lazy.layout.down()),
    Key([mod], 'j', lazy.layout.up()),

    # Move windows up or down in current stack
    Key([mod, 'shift'], 'k', lazy.layout.shuffle_down()),
    Key([mod, 'shift'], 'j', lazy.layout.shuffle_up()),

    # Switch window focus to other pane(s) of stack
    Key([mod], 'Tab', lazy.layout.next()),

    # Move window to next stack
    Key([mod, 'shift'], 'Tab', lazy.layout.client_to_next()),

    # Close window
    Key([mod, 'shift'], 'w', lazy.window.kill()),

    # Toggle between split and unsplit stack
    Key([mod], 'n', lazy.layout.toggle_split()),

    # Switch screen
    Key([mod], 'l', lazy.to_screen(0)),
    Key([mod], 'h', lazy.to_screen(1)),

    # Launch applications
    Key([mod], 'Return', lazy.spawn('xterm')),
    Key([mod], 'space', lazy.spawn('dmenu_run {}'.format(dmenu_options))),

    # Choose layout
    Key([mod], 'm', lazy.group.setlayout('max')),
    Key([mod], 's', lazy.group.setlayout('stack2')),
    Key([mod, 'shift'], 's', lazy.group.setlayout('stack3')),
    Key([mod], 'r', lazy.group.setlayout('ratiotile')),

    # Sound volume
    Key([mod], 'F6', lazy.spawn('amixer -c 0 set Master 1-')),
    Key([mod], 'F7', lazy.spawn('amixer -c 0 set Master 1+')),
    Key([mod], 'F8', lazy.spawn('alsactl -f ~/.asound.state restore')),
    Key([], 'XF86AudioMute', lazy.spawn('amixer -c 0 set Master mute')),
    Key(['shift'],
        'XF86AudioMute',
        lazy.spawn('amixer -c 0 set Master unmute')),

    # Restart
    Key([mod, "control"], "r", lazy.restart()),
]


####
# Group and application configuration

groups = []


def app_or_group(group, app):
    """ Go to specified group if it exists. Otherwise, run the specified app.
    When used in conjunction with dgroups to auto-assign apps to specific
    groups, this can be used as a way to go to an app if it is already
    running. """
    def f(qtile):
        try:
            qtile.groupMap[group].cmd_toscreen()
        except KeyError:
            qtile.cmd_spawn(app)
    return f


groups.append(
    Group(
        'Terminal',
        init=True,
        persist=True,
        exclusive=True,
        layout='ratiotile',
        matches=[Match(wm_class=['XTerm'])]
    )
)
keys.append(
    Key(
        [mod], 't',
        lazy.function(app_or_group('Terminal', 'xterm -e ~/conf/tmux/tmux.sh'))
    )
)

groups.append(
    Group(
        'WWW',
        init=True,
        persist=False,
        exclusive=True,
        matches=[Match(wm_class=['Google-chrome-beta'])]
    )
)
keys.append(
    Key([mod], 'w', lazy.function(app_or_group('WWW', 'google-chrome-beta')))
)

groups.append(
    Group(
        'Skype',
        init=False,
        persist=False,
        exclusive=False,
        layout='stack2',
        matches=[Match(wm_class=['Skype'])]
    )
)
keys.append(
    Key([mod], 'c', lazy.function(app_or_group('Skype', 'skype')))
)

dgroups_key_binder = simple_key_binder(mod)


####
# Layouts

layout_defaults = dict(
    border_normal=BACKGROUND,
    border_focus=HIGHLIGHT,
    border_width=3
)

layouts = [
    layout.Max(
        name='max',
        **layout_defaults
    ),
    layout.Stack(
        name='stack2',
        num_stacks=2,
        autosplit=True,
        **layout_defaults
    ),
    layout.Stack(
        name='stack3',
        num_stacks=3,
        autosplit=True,
        **layout_defaults
    ),
    layout.RatioTile(
        name='ratiotile',
        **layout_defaults
    ),
]

floating_layout = layout.Floating(
    auto_float_types=[
        'utility',
        'notification',
        'toolbar',
        'splash'
    ]
)


####
# Screens

widget_defaults = dict(
    font='Envy Code R',
    fontsize=14,
    foreground=BAR_TEXT,
)

groupbox_defaults = dict(
    font='Envy Code R',
    fontsize=11,
    rounded=False,
    active=BAR_TEXT,
    inactive=BAR_TEXT,
    urgent_border=URGENT,
    urgent_text=URGENT_TEXT,
    this_current_screen_border=HIGHLIGHT,
    this_screen_border=SECONDARY_HIGHLIGHT,
    other_screen_border=OTHER_SCREEN,
)

bar_defaults = dict(
)

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.GroupBox(**groupbox_defaults),
                widget.WindowName(**widget_defaults),
                widget.Systray(**widget_defaults),
                netctl.NetctlStatus(**widget_defaults),
                widget.Battery(
                    format='{char} {percent:2.0%} {hour:d}:{min:02d}',
                    charge_char='C',
                    discharge_char='D',
                    **widget_defaults),
                widget.Clock('%a %-d %b %H:%M', **widget_defaults),
            ],
            24,
            background=BACKGROUND
        )
    ),
    Screen(
        bottom=bar.Bar(
            [
                widget.GroupBox(**groupbox_defaults),
                widget.WindowName(**widget_defaults),
            ],
            24,
            background=BACKGROUND
        )
    )
]


@hook.subscribe.screen_change
def restart_on_screen_change(qtile, event):
    """Restart on screen change."""
    qtile.cmd_restart()


####
# Mouse

follow_mouse_focus = True
cursor_warp = False
mouse = ()
