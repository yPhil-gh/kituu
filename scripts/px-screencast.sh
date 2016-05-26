#!/usr/bin/env bash

FILE="${1:-}"

if [[ -e "$FILE.mp4" ]] ; then
    echo "file exists"
    exit
fi

ffmpeg -f x11grab  -s 1920x1200 -i :0.0 -vcodec libx264 "$FILE.mp4" &
jack_capture -c 2 -p system:capture_1 -p Qtractor:Master/out_1 -p Qtractor:Master/out_2 "$FILE.wav"
wait
