#!/usr/bin/env sh

if [ ! -d $objects_dir/mu ]; then
    git clone https://github.com/djcb/mu.git $objects_dir/mu
    cd $objects_dir/mu
    git checkout tags/v0.9.16
fi

cd $objects_dir/mu
autoreconf -i
./configure --prefix=$local_prefix_dir --disable-gtk
make -j4
