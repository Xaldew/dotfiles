#!/usr/bin/env sh
# Installs all dotfiles into the autostart folder specified by XDG.

add_autostart_files()
{
    dest=$1
    for file in $dotfiles_dir/autostart/*; do
	cp $file $dest
    done
    return 0
}

if [ -d "$XDG_CONFIG_HOME/autostart" ]; then
    echo "autostart exists."
    add_autostart_files $XDG_CONFIG_HOME/autostart
elif [ -d $HOME/.config/autostart ]; then
    echo "XDG_CONFIG_HOME not set, but ~/.config/autostart exists."
    add_autostart_files $HOME/.config/autostart
else
    echo "XDG_CONFIG_HOME not set, and ~/.config/autostart does not exist."
    echo "Creating ~/.config/autostart..."
    mkdir -p $HOME/.config/autostart
    add_autostart_files $HOME/.config/autostart
fi
