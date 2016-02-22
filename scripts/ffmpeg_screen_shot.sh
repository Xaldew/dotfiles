#!/usr/bin/env sh
# Take a screenshot with ffmpeg.

output=${1-out.jpg}

if [ "$OSTYPE" = "cygwin" ]; then
    size='1920x1200'
    device="dshow"
    input="video='screen-capture-recorder"
else
    size=`xrandr | grep "*" | awk '{ print $1 }'`
    device="x11grab"
    input=$DISPLAY
fi

ffmpeg -loglevel error -s $size -f $device -i $input -frames:v 1 $output
