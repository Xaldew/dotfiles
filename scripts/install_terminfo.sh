#!/usr/bin/env sh
# Create a local `terminfo` database with more modern entries.
tmp=$(mktemp --directory)
url="http://invisible-island.net/datafiles/current/terminfo.src.gz"
file=$(basename $url)
wget --quiet $url --output-document=$tmp/$file
gunzip -c $tmp/$file | tic -x -
rm -rf $tmp
tic -x $dotfiles_dir/configs/terminfo.src
