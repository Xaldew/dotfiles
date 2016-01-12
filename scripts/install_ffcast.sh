#!/usr/bin/env sh
# Install ffcast: A screencasting tool.

git clone --recursive \
    https://github.com/lolilolicon/FFcast.git \
    $objects_dir/ffcast
cd $objects_dir/ffcast
./bootstrap
./configure --prefix=$local_prefix_dir --enable-xrectsel
make -j4
make install
