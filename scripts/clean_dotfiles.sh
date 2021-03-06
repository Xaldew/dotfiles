#!/usr/bin/env sh
# Clean up all previously installed dotfiles, fonts and autostart files.

echo "Removing autostart files..."
if [ -d "$XDG_CONFIG_HOME/autostart" ]; then
    rm $XDG_CONFIG_HOME/autostart/*
elif [ -d $HOME/.config/autostart ]; then
    rm -f $HOME/.config/autostart/*
else
    rm -f $HOME/.config/autostart/*
fi

echo "Removing installed fonts..."
rm $HOME/.fonts/*

echo "Removing dotfiles..."
rm $HOME/.inputrc
rm $HOME/.bashrc
rm $HOME/.bash_aliases
rm $HOME/.gitconfig
rm $HOME/.gitignore
rm $HOME/.tmux.conf
rm $HOME/.Xresources
rm $HOME/.latexmkrc
rm $HOME/.emacs
rm -r $HOME/.emacs.d/
rm $HOME/.vimrc
rm -r $HOME/.vim
rm $HOME/.dicts

echo "Removing all objects..."
rm -rf $objects_dir

echo "Removing all locally installed binaries..."
rm -rf $local_prefix_dir
