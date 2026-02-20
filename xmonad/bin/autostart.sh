## Autostart Programs

# killall -q polybar & 

# Kill already running process
_ps=(picom dunst ksuperkey mpd xfce-polkit xfce4-power-manager)
for _prs in "${_ps[@]}"; do
	if [[ `pidof ${_prs}` ]]; then
		killall -9 ${_prs}
	fi
done
u

# keyboard speed
xset r rate 165 75 &

# Fix cursor
xsetroot -cursor_name left_ptr

# Polkit agent
/usr/lib/xfce-polkit/xfce-polkit &

# Enable power management
xfce4-power-manager &

# Restore wallpaper
hsetroot -cover ~/Pictures/highland.jpg

# Lauch polybar
(sleep 0.1; ~/.xmonad/bin/xmobar.sh) &

# Start mpd
exec mpd &

# Set correct audio sink for volume controls
pactl set-default-sink alsa_output.pci-0000_07_00.6.HiFi__Speaker__sink &
