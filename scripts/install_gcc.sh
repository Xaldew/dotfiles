#!/usr/bin/env sh
# Install the GNU Compiler Collection.

sudo apt-get install \
     libgmp-dev \
     libmpfr-dev \
     libmpc-dev

cd $objects_dir
if [ -d $objects_dir/gcc ]; then
    cd $objects_dir/gcc
    git pull
else
    git clone --depth 1000 git://gcc.gnu.org/git/gcc.git
fi

cd $objects_dir/gcc
./configure --prefix=$local_prefix_dir \
	    --mandir=$local_prefix_dir/share/man \
	    --enable-gold=yes \
	    --enable-lto
make -j4
make install
