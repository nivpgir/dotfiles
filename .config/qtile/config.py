
# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import asyncio

from libqtile import bar, layout, widget, hook, qtile
from libqtile.core.manager import Qtile
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal, send_notification
from libqtile.log_utils import logger

from popups import show_keybindings, show_logs
from input_config import wl_input_rules

qtile.cmd_debug()

kb_config = wl_input_rules["type:keyboard"]


@hook.subscribe.startup_once
def startup():
    qtile.cmd_spawn("kanshi")


terminal = guess_terminal()

mod = "mod4"
keys = [
    Key([mod], "grave", lazy.function(show_keybindings)),
    # Key([mod], "slash", lazy.function(show_logs)),
    Key([mod], "slash", lazy.spawn(
        "foot -W 130x40 -a floating-terminal -e bat --wrap=never --paging=always /home/piamh/.local/share/qtile/qtile.log"
        # "foot -W 130x40 -a floating-terminal -e bat --wrap=never --paging=always --pager='less +G -R'  /home/piamh/.local/share/qtile/qtile.log"
        # "foot -a floating-terminal -e  bat --pager 'less -RF +G' /home/piamh/.local/share/qtile/qtile.log"
        # "foot -a floating-terminal -e  bat --pager 'less +G -RF' ~/.local/share/qtile/qtile.log"
    )),

    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod], "Left", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "Right", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "Down", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "Up", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "Tab", lazy.layout.next(), desc="Move window focus to other window"),
    Key([mod, "control"], "Tab", lazy.group.next_window(), desc="Cycle focus between group windows"),
    Key([mod], "t", lazy.window.toggle_floating(), desc="Toggle floating status"),
    Key([mod], "f", lazy.window.toggle_maximize(), desc="Toggle fullscreen status"),
    Key([mod, "shift"], "f", lazy.window.toggle_fullscreen(), desc="Toggle fullscreen status"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "Left", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "Right", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "Down", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "Up", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "Left", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "Right", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "Down", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "Up", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod], "e", lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod], "g", lazy.spawn("quick-snote"), desc="New a journal entry"),
    # Toggle between different layouts as defined below
    Key([mod, "shift"], "semicolon", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),

    *[
        Key(["control", "mod1"],
            f"F{i}",
            lazy.core.change_vt(i),
            desc=f"Go to virtual console {i}")
        for i in range(1,6)
     ],
    # Key(["control", "mod1"], "F1", lazy.core.change_vt(1), desc="Go to virtual console 1"),
    # Key(["control", "mod1"], "F2", lazy.core.change_vt(2), desc="Go to virtual console 2"),
    # Key(["control", "mod1"], "F3", lazy.core.change_vt(3), desc="Go to virtual console 3"),
    # Key(["control", "mod1"], "F7", lazy.core.change_vt(4), desc="Go to virtual console 7"),
    # Key(["control", "mod1"], "F4", lazy.core.change_vt(5), desc="Go to virtual console 4"),
    # Key(["control", "mod1"], "F5", lazy.core.change_vt(6), desc="Go to virtual console 5"),
    # Key(["control", "mod1"], "F6", lazy.core.change_vt(7), desc="Go to virtual console 6"),
]

groups = [Group(i) for i in "12"]


@hook.subscribe.setgroup
def delete_empty_group():
    for group in qtile.groups:
        if group == qtile.current_group:
            continue
        if len(group.windows) == 0:
            qtile.delete_group(group.name)


def create_and_show_group(qtile, group_name):
    if group_name not in qtile.groups:
        qtile.add_group(group_name)

    qtile.groups_map[group_name].cmd_toscreen()


for i in "1234567890":
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i,
                lazy.function(create_and_show_group, i),
                desc="Switch to group {}".format(i),
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i,
                # lazy.function(lambda q, i=i: q.move_to_group(i)),
                lazy.function(Qtile.move_to_group, i),
                desc="Switch to & move focused window to group {}".format(i),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

layouts = [
    layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=4),
    layout.Max(),
    # layout.Matrix(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="sans",
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

default_screen = Screen(
    top=bar.Bar(
        [
            widget.CurrentLayout(),
            widget.GroupBox(),
            widget.Prompt(),
            widget.WindowName(),
            widget.Chord(
                chords_colors={
                    "launch": ("#ff0000", "#ffffff"),
                },
                name_transform=lambda name: name.upper(),
            ),
            widget.Clock(format="%Y-%m-%d %a %I:%M %p", fontsize=20),
            widget.WidgetBox(widgets=[
                widget.Memory(),
                widget.NetGraph(),
                widget.Wlan(interface="wlp3s0"),
                widget.Bluetooth(),
                widget.BatteryIcon(),
                widget.Battery(),
            ]),
            widget.LaunchBar(progs=[("Terminal", "alacritty")]),
            # widget.KeyboardLayout(configured_keyboards=kb_config.kb_layout.split(","),
            #                       option=kb_config.kb_options),
            widget.CapsNumLockIndicator(),
            widget.StatusNotifier(),
            widget.QuickExit(),
        ],
        24,
        border_width=[2, 0, 2, 0],  # Draw top and bottom borders
        # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
    ),
)
screens = [ default_screen ]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
    Click([mod], "Button4", lazy.layout.next()),
    Click([mod], "Button5", lazy.layout.previous()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(wm_class="floating-terminal"),
        Match(title="QuickSnoTT"),
    ]
)

auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True


# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"

@hook.subscribe.screens_reconfigured
async def outputs_changed():
    logger.warning("Screens reconfigured")
    await asyncio.sleep(1)
    logger.warning("Reloading config...")
    qtile.cmd_reload_config()
