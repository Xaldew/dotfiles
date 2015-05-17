#!/usr/bin/env sh
# This script creates a folder for the development release of tmux,
# then compiles and installs it.

sudo apt-get --quiet=2 install libevent-dev libncurses5-dev
cd $objects_dir
if [ -d $objects_dir/tmux ]; then
    cd $objects_dir/tmux
    make distclean
    git clean --force
    git pull
else
    git clone git://git.code.sf.net/p/tmux/tmux-code tmux
fi

cd $objects_dir/tmux
sh autogen.sh
./configure \
    --prefix=$local_prefix_dir \
    --mandir=$local_prefix_dir/share/man \
    --enable-static
make -j4
make install
