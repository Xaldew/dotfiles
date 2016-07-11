#!/usr/bin/env sh
# Install latest version of rtags the C/C++ symbol indexer.

cd $objects_dir
if [ -d $objects_dir/rtags ]; then
    cd $objects_dir/rtags
    git pull
else
    git clone --recursive \
        https://github.com/Andersbakken/rtags.git $objects_dir/rtags
fi

cd $objects_dir/rtags
cmake \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=1 \
    -DCMAKE_INSTALL_PREFIX=$local_prefix_dir \
    .
make
make install
