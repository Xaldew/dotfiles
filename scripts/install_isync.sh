#!/usr/bin/env sh

if [ ! -d $objects_dir/isync ]; then
    cd $objects_dir
    git clone https://git.code.sf.net/p/isync/isync ${objects_dir}/isync
    cd $objects_dir/isync
fi

cd $objects_dir/isync
sh autogen.sh

./configure \
    --prefix=$local_prefix_dir
make -j4
make install
