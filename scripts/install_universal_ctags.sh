#!/usr/bin/env sh
# Install latest universal ctags and rename it appropriately.

cd $objects_dir
if [ -d $objects_dir/universal-ctags ]; then
    cd $objects_dir/universal-ctags
    git pull
else
    git clone \
        https://github.com/universal-ctags/ctags.git \
        $objects_dir/universal-ctags
fi

cd $objects_dir/universal-ctags

./autogen.sh
./configure \
    --prefix=$local_prefix_dir \
    --mandir=$local_prefix_dir/share/man \
    --program-prefix="uni-" \
    --disable-etags
make -j4
make install
