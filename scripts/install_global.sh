#!/usr/bin/env sh
# Install a specific release of GNU global (gtags).

# DEB: sudo apt-get bison flex gperf


cd $objects_dir
if [ ! -d $objects_dir/global ]; then
    url="ftp://ftp.gnu.org/pub/gnu/global/global-6.5.5.tar.gz"
    file=`basename $url`
    wget --quiet $url --output-document=$file
    tar -zxf $file --transform='s/-6.5.5//g'
    rm $file
fi

cd $objects_dir/global
sh reconf.sh
./configure \
    --prefix=$local_prefix_dir \
    --mandir=$local_prefix_dir/share/man \
    --with-exuberant-ctags=uctags
    --with-universal-ctags=uctags
make -j4
make install
