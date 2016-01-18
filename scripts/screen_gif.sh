#!/usr/bin/env bash
# Record a selected screen region and output it as a GIF.

tmp_avi=$(mktemp /tmp/outXXXXXXXXXX.avi)
ffcast -s % ffmpeg -y -f x11grab -show_region 1 -framerate 15 \
       -video_size %s -i %D+%c -codec:v huffyuv               \
       -vf crop="iw-mod(iw\\,2):ih-mod(ih\\,2)" $tmp_avi      \
    && convert -set delay 10 -layers Optimize $tmp_avi out.gif
rm $tmp_avi
