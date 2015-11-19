#!/usr/bin/env sh
# Install Python2.7 and Python3


url=https://www.python.org/ftp/python/2.7.10/Python-2.7.10.tgz
if [ ! -d $objects_dir/python27 ]; then
    cd $objects_dir
    file=`basename $url`
    wget --quiet $url
    tar xf $file
    dir=`basename $file .tgz`
    rm $file
    mv $dir python27
    dir=python27
    cd $dir
    ./configure --prefix=$local_prefix_dir --enable-unicode=ucs4 --enable-shared
    make -j4
    make altinstall
fi

url=https://www.python.org/ftp/python/3.5.0/Python-3.5.0.tgz
if [ ! -d $objects_dir/python3 ]; then
    cd $objects_dir
    file=`basename $url`
    wget --quiet $url
    tar xf $file
    dir=`basename $file .tgz`
    rm $file
    mv $dir python3
    dir=python3
    cd $dir
    ./configure --prefix=$local_prefix_dir --enable-shared
    make -j4
    make altinstall
fi
