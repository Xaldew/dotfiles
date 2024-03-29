#!/usr/bin/env bash
# Record a selected screen region and output it as a GIF.

out=${1:-out.gif}
fps=${2:-15}
avi=${3-$(mktemp XXXX_XXXX.avi)}

xcb_rectsel | {
    read xstart xend width height
    ffmpeg -y -f x11grab -show_region 1 -framerate 15 \
           -video_size ${width}x${height} -i ${DISPLAY:-":0.0"}+${xstart},${xend} -codec:v huffyuv               \
           -vf crop="iw-mod(iw\\,2):ih-mod(ih\\,2)" $avi
    clear
    if [ $? -eq 0 ]; then
        palette=$(mktemp XXXX_XXXX.png)
        ffmpeg -y -loglevel error -i $avi -filter:v palettegen $palette
        ffmpeg -y -loglevel error -i $palette -i $avi \
               -filter_complex "[1:v][0:v] paletteuse" $out
        rm $avi $palette
    fi
}
