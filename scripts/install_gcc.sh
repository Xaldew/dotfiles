#!/usr/bin/env sh
# Install the GNU Compiler Collection.

# DEB: sudo apt-get install libgmp-dev libmpfr-dev libmpc-dev

cd $objects_dir
if [ -d $objects_dir/gcc ]; then
    cd $objects_dir/gcc
    git pull
else
    git clone git://gcc.gnu.org/git/gcc.git
    git checkout -B v5.2.0 tags/gcc-5_2_0-release
    $objects_dir/gcc/contrib/contrib/download_prerequisites
fi

cd $objects_dir/gcc
./configure --prefix=$local_prefix_dir \
	    --mandir=$local_prefix_dir/share/man \
            --program-suffix=5.2 \
	    --enable-gold=yes \
	    --enable-lto
make -j4
make install
