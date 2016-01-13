#!/usr/bin/env sh
# Install latest static release of ffmpeg.

tmp=`mktemp --directory`
cd $tmp
wget http://johnvansickle.com/ffmpeg/releases/ffmpeg-release-64bit-static.tar.xz
tar xf ffmpeg-release-64bit-static.tar.xz --strip-components=1
for f in *;
do
    if [ -x $f -a -f $f ]; then
       mv $f $local_prefix_dir/bin
    fi
done
rm -r $tmp
