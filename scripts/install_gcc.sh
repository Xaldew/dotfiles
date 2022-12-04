#!/usr/bin/env sh
# Install the GNU Compiler Collection.

# DEB: sudo apt-get install libgmp-dev libmpfr-dev libmpc-dev
# RPM: sudo yum install gmp-devel mpfr-devel libmpc-devel

cd $objects_dir
if [ -d $objects_dir/gcc ]; then
    cd $objects_dir/gcc
    git pull
else
    git clone git://gcc.gnu.org/git/gcc.git
    $objects_dir/gcc/contrib/contrib/download_prerequisites
fi

cd $objects_dir/gcc
./configure \
    --prefix=$local_prefix_dir \
    --program-suffix="-master" \
    --enable-gold=yes \
    --enable-lto &&
    make -j4 &&
    make install
