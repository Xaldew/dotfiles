#!/usr/bin/env bash
# @TODO: Actually install the autostart files.

if [ -d "$XDG_CONFIG_HOME/autostart" ]; then
    echo "autostart exists."
    for file in $DOTFILES_DIR/autostart/*; do
	echo $file
    done
else
    echo "XDG_CONFIG_HOME propbably not set, or autostart does not exist."
fi
