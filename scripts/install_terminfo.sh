#!/usr/bin/env bash
# Create a local `terminfo` database with more modern entries.
# Figure out where we have placed the script.
dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

tmp=$(mktemp -d)
url="http://invisible-island.net/datafiles/current/terminfo.src.gz"
file=$(basename $url)
wget --quiet $url --output-document=$tmp/$file
gunzip -c $tmp/$file | tic -x -
rm -rf $tmp
tic -x ${dir}/terminfo.src
