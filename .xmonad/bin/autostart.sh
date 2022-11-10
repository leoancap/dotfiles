#!/usr/bin/env bash

## Copyright (C) 2020-2021 Aditya Shakya <adi1090x@gmail.com>
## Everyone is permitted to copy and distribute copies of this file under GNU-GPL3
## Autostart Programs

# Kill already running process
_ps=(picom dunst ksuperkey mpd xfce-polkit xfce4-power-manager)
for _prs in "${_ps[@]}"; do
	if [[ `pidof ${_prs}` ]]; then
		killall -9 ${_prs}
	fi
done
u

# disable middle-mouse click altogether
xmodmap -e "pointer = 1 25 3 4 5 6 7 8 9 10" & 
# increase trackpoint speed
xinput --set-prop 19 'libinput Accel Speed' 1 &
xinput --set-prop 10 'libinput Accel Speed' 1 &

# set caps to ctrl/Escape
xset r rate 175 45 &
setxkbmap us -option 'caps:ctrl_modifier' &
(sleep 0.01; xmodmap $HOME/.xmodmaprc) &

# Fix cursor
xsetroot -cursor_name left_ptr

# Polkit agent
/usr/lib/xfce-polkit/xfce-polkit &

# Enable power management
xfce4-power-manager &

# Enable Super Keys For Menu
# ksuperkey -e 'Super_L=Alt_L|F1' &
# ksuperkey -e 'Super_R=Alt_L|F1' &

# Restore wallpaper
hsetroot -cover ~/Pictures/highland.jpg

# Lauch notification daemon
~/.xmonad/bin/xmodunst.sh

# Lauch polybar
~/.xmonad/bin/xmobar.sh

# Lauch compositor
~/.xmonad/bin/xmocomp.sh

# Start mpd
exec mpd &
