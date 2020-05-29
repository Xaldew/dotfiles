#!/usr/bin/env sh
# Install latest static release of ffmpeg.

tmp=`mktemp --directory`
cd $tmp
wget https://johnvansickle.com/ffmpeg/releases/ffmpeg-release-amd64-static.tar.xz
tar xf ffmpeg-release-amd64-static.tar.xz --strip-components=1
for f in *;
do
    if [ -x $f -a -f $f ]; then
        mv $f $local_prefix_dir/bin
    fi
done
rm -r $tmp
