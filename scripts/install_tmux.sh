#!/usr/bin/env sh
# This script creates a folder for the development release of tmux,
# then compiles and installs it.

sudo apt-get --quiet=2 install libevent-dev libncurses5-dev
if [ -d $objects_dir/tmux ]; then
    cd $objects_dir/tmux
    make distclean
    git clean --force
    git pull
else
    cd $objects_dir
    git clone https://github.com/tmux/tmux.git tmux
fi

cd $objects_dir/tmux
sh autogen.sh
./configure \
    --prefix=$local_prefix_dir \
    --mandir=$local_prefix_dir/share/man \
    --enable-static
make -j4
make install
