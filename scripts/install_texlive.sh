#!/usr/bin/env sh
# Install texlive installer.

url=http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
wget --quiet $url --output-document=$objects_dir/install-tl.tar.gz
tar -xvzf $objects_dir/install-tl.tar.gz -C $objects_dir
