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

import os
import subprocess

from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen, ScratchPad, DropDown
from libqtile.lazy import lazy

mod = "mod4"
terminal = "alacritty"

colours = {
    "persian": "#339989",
    "space": "#1F363D",
    "tea": "#D0EFB1",
    "sugar": "#B4654A",
    "rajah": "#F6AF65"
}

keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    # Key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    Key([mod], "space", lazy.spawn("rofi -show run"), desc="Launch Rofi Run Window"),
    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
]

# groups = [Group(i) for i in "123456789"]
groups = [Group(i) for i in ["", "", "", ""]]
group_hotkeys="1234"

for g, k in zip(groups, group_hotkeys):
    keys.extend([
        Key(
            [mod],
            k,
            lazy.group[g.name].toscreen(),
            desc=f"Switch to group {g.name}"
        ),
        Key(
            [mod, "shift"],
            k,
            lazy.window.togroup(g.name, switch_group=True),
            desc=f"Switch to group {g.name} and move current window"
        )
        ])

groups.append(ScratchPad("scratchpad", [DropDown("term", "alacritty")]))
keys.extend([Key([mod], "p", lazy.group['scratchpad'].dropdown_toggle('term'))])

layouts = [
    #layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=4),
    #layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    layout.MonadTall(
        margin=5,
        border_width=2,
        border_focus=colours["rajah"],
        border_normal=colours["sugar"]
    ),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="TerminessTTF Nerd Font Mono"
)

extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=40,
                    foreground=colours["persian"],
                    background="#00000000",
                    ),
                widget.GroupBox(
                    highlight_method="line",
                    fontsize=30,
                    background=colours["persian"],
                    this_current_screen_border=colours["tea"],
                    highlight_color=[colours["persian"], colours["persian"]],
                    inactive="#000000",
                    active="#ffffff",
                    center_aligned=True,
                    disable_drag=True,
                    rounded=True,
                    urgent_alert_method="line",
                    urgent_border=colours["sugar"]
                    ),
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=40,
                    foreground=colours["persian"],
                    background=colours["space"],
                    ),
                widget.Spacer(background=colours["space"], length=10),
                widget.WindowName(background=colours["space"]),
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=40,
                    foreground=colours["persian"],
                    background=colours["space"],
                ),
                widget.Systray(
                    background=colours["persian"]),
                widget.Spacer(length=5, background=colours["persian"]),
                widget.CheckUpdates(
                    distro='Arch',
                    no_update_string="",
                    display_format="",
                    background=colours["persian"]),
                widget.TextBox(
                    text="",
                    fontsize=30,
                    padding=5,
                    background=colours["persian"],
                    foreground="#ffffff"
                ),
                widget.Clock(
                    padding=0,
                    format="%H:%M %a %Y-%m-%d",
                    background=colours["persian"]
                ),
                widget.TextBox(
                    text="",
                    padding=0,
                    fontsize=40,
                    foreground=colours["persian"],
                    background="#00000000",
                )
            ],
            24,
            #border_width=[0, 0, 10, 0],  # Draw top and bottom borders
            background=colours["space"]
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
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
    ]
)

@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/scripts/qtile_startup.sh')
    subprocess.Popen([home])


auto_fullscreen = False
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
