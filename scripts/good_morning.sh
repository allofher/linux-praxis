#!/bin/bash
#
# This script starts various programs within specific groups in qtile. This gets us our preferred layout of working windows.
# The documentation for the commands can be found at...
qtile run-cmd -g 1 emacsclient -c &
qtile run-cmd -g 2 firefox &
qtile run-cmd -g 3 tidal-hifi &
qtile run-cmd -g 3 discord-ptb &
qtile run-cmd -g 4 alacritty -e "htop" &
qtile run-cmd -g 4 thunderbird &
