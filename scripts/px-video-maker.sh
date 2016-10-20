#!/usr/bin/env bash

if [ "$#" -ne 5 ]; then
    echo "Usage: $0 [audio file path] 'artist name' 'album name' [artwork image file path] [point|line|p2p|cline]"
    exit
fi

FILE_PATH="$1"
ARTIST="$2"
ALBUM="$3"
COVER_FILE_PATH="$4"
EFFECT="$5"
FFMPEG="ffmpeg"

convert -resize 1280x720 "${COVER_FILE_PATH}" -background none -gravity center -extent 1280x720 "${COVER_FILE_PATH}.jpg"

$FFMPEG -y -i "${FILE_PATH}" -loop 1 -i "${COVER_FILE_PATH}.jpg" -filter_complex "[0:a] showwaves=s=1280x310:mode=${EFFECT}:r=25,colorkey=0x000000:0.1:0.5[wave];[1:v][wave] overlay=y=H-h:eval=init[canvas];[canvas]drawtext=fontfile='Jura-Book.ttf':fontsize=60:text='${ARTIST}':x=30:y=(h-70-text_h*2.5):fontcolor=white:shadowy=2:shadowx=2:shadowcolor=black,drawtext=fontfile='Jura-Book.ttf':fontsize=72:text='${ALBUM}':x=20:y=(h-70-text_h):fontcolor=ff6600:shadowy=2:shadowx=2:shadowcolor=black" -shortest -codec:v libx264 -crf 18 -preset fast -codec:a aac -strict -2 -b:a 192k "${FILE_PATH}-composite.mp4"

if [[ -e "end.mp4" ]]; then
    $FFMPEG -y -i "${FILE_PATH}-composite.mp4" -i "end.mp4" -filter_complex "[0:0] [0:1] [1:0] [1:1] concat=n=2:v=1:a=1 [v] [a]" -map "[v]" -map "[a]" "${ARTIST}-${ALBUM}.mp4"

    rm -rf "${FILE_PATH}-composite.mp4"
else
    mv "${FILE_PATH}-composite.mp4" "${ARTIST}-${ALBUM}.mp4"
fi

rm -rf "${COVER_FILE_PATH}.jpg"

echo -e "
################################################
DONE : ${ARTIST}-${ALBUM}.mp4"
