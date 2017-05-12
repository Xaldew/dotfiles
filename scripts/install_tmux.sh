#!/usr/bin/env sh
# This script creates a folder for the development release of tmux,
# then compiles and installs it.

# DEB: sudo apt-get install libevent-dev libncurses5-dev
# RPM: sudo yum install libevent-devel ncurses-devel

if [ ! -d $objects_dir/libevent ]; then
    tmp=`mktemp --directory`
    cd $tmp
    url="https://github.com/libevent/libevent/releases/download/release-2.0.22-stable/"
    dir="libevent-2.0.22-stable"
    file=$dir".tar.gz"
    wget --quiet --output-document=$file $url$file
    tar -xvzf $file
    mv $dir $objects_dir/libevent
    cd $objects_dir/libevent
    sh autogen.sh
    ./configure \
	--prefix=$local_prefix_dir \
	--mandir=$local_prefix_dir/share/man
    make -j8 install
    rm -r $tmp
fi

if [ ! -d $objects_dir/tmux ]; then
    cd $objects_dir
    git clone https://github.com/tmux/tmux.git tmux
    cd $objects_dir/tmux
    git checkout tags/2.3
fi

cd $objects_dir/tmux
sh autogen.sh

env LIBEVENT_CFLAGS="-I${local_prefix_dir}/include" \
    LIBEVENT_LIBS="-L${local_prefix_dir}/lib -levent" \
    ./configure \
    --prefix=$local_prefix_dir \
    --mandir=$local_prefix_dir/share/man
make -j4
make install
