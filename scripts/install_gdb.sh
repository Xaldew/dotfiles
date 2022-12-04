#!/usr/bin/env sh
# Install the latest development release of Gdb.

# Ubuntu: help2man libpython2-dev libmpfr-dev

if [ ! -d $objects_dir/gdb ]; then
    git clone https://sourceware.org/git/binutils-gdb.git $objects_dir/gdb
else
    cd $objects_dir/gdb
    git pull
fi

# Compile and install the latest master version of gdb.
cd $objects_dir/gdb &&
    ./configure \
        --prefix="$local_prefix_dir" \
        --with-python=/usr/bin/python3 \
        --with
        --program-suffix="-master" &&
    make &&
    make install
