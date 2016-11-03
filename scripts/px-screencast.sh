#!/usr/bin/env bash

# trap killgroup SIGINT

# killgroup(){
#   echo killing...
#   killall 0
# }

FILE="${1:-}"

if [[ -e "$FILE.mp4" ]] ; then
    read -e -p "## File ($FILE) exists, delete? [y/N] " YN
    if [[ $YN == "n" || $YN == "N" || $YN == "" ]] ; then
        exit
    else
        rm -rf "$FILE"*
    fi
fi

# key-mon --visible_click -l --backgroundless -t modern &
# screenkey &
ffmpeg -f x11grab -r 30 -s 1855x1200 -i :0.0+65,0 "$FILE.mp4" &
jack_capture "$FILE.wav"
# jack_capture -c 2 -p system:capture_1 -p Qtractor:Master/out_1 -p Qtractor:Master/out_2 "$FILE.wav"
wait
