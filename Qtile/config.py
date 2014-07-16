from libqtile.config import Key, Screen, Group
from libqtile.command import lazy
from libqtile import layout, hook, bar, widget


####
# Key bindings

dmenu_options = "-i -fn 'Inconsolata-14'"

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
    Key([mod], 'Return', lazy.spawn('st')),
    Key([mod], 'space', lazy.spawn('dmenu_run {}'.format(dmenu_options))),

    # Choose layout
    Key([mod], 'm', lazy.group.setlayout('max')),
    Key([mod], 't', lazy.group.setlayout('stack')),
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
# Groups

groups = [
    Group("a"),
    Group("s"),
    Group("d"),
    Group("f"),
    Group("u"),
    Group("i"),
    Group("o"),
    Group("p"),
]
for i in groups:
    # mod4 + letter of group = switch to group
    keys.append(Key([mod], i.name, lazy.group[i.name].toscreen()))

    # mod4 + shift + letter of group = switch to & move focused window to group
    keys.append(Key([mod, "shift"], i.name, lazy.window.togroup(i.name)))


####
# Layouts

layouts = [
    layout.Stack(stacks=2, border_focus='Firebrick4', border_width=2),
    layout.RatioTile(border_focus='SteelBlue4', border_width=2),
    layout.Max(),
]

floating_layout = layout.Floating(auto_float_types=[
    'utility',
    'notification',
    'toolbar',
    'splash'])


####
# Screens

widget_defaults = dict(font='Inconsolata', fontsize=18)

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.GroupBox(
                    font='Inconsolata',
                    fontsize=12,
                    borderwidth=3),
                widget.WindowName(**widget_defaults),
                widget.Systray(**widget_defaults),
                widget.Battery(
                    format='{char} {percent:2.0%} {hour:d}:{min:02d}',
                    charge_char='C',
                    discharge_char='D',
                    **widget_defaults),
                widget.Clock('%a %-d %b %H:%M', **widget_defaults),
            ],
            32,
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
