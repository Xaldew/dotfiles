#!/usr/bin/env sh
# Install Python2.7 and Python3

(
    url=https://www.python.org/ftp/python/2.7.10/Python-2.7.10.tgz
    if [ ! -d $objects_dir/python27 ]; then
        tmp=`mktemp --directory`
        cd $tmp
        wget --quiet $url
        rm -r $tmp
    fi
) &

(
    url=https://www.python.org/ftp/python/3.5.0/Python-3.5.0.tgz
    if [ ! -d $objects_dir/python3 ]; then
        tmp=`mktemp --directory`
        :
        rm -r $tmp
    fi
) &
