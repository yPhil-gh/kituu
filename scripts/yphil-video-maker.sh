#!/usr/bin/env bash

usage() {
cat <<-EOM
Usage: $(basename $0) OPTIONS
  -w Wave file (required)
  -i Image (required)
  -a Artist name (optional)
  -t Title of song or album (optional)
  -s Style of waveform (optional). Can be point, line, p2p, or cline. Default is line.
  -S Style color (optional). Defaults to 
  -o Override the output file name (optional). Otherwise, -a and -i are used to construct file name.
EOM
}

error() {
  echo Error: $@
  usage
  exit 1
}

#[[ $# -eq 3 ]] || { usage; exit 1 }

FFMPEG="ffmpeg"
STYLE="line"
IMAGE_TMP=$(mktemp $(basename $0).XXXXXXXXXX)
MP4_TMP=$(mktemp $(basename $0).XXXXXXXXXX.mp4)

while getopts 'w:i:a:t:s:o:' OPTION; do
  case $OPTION in
    w)
      WAV=$OPTARG
      [[ -f $WAV ]] || error $WAV is not a file.
      ;;
    i)
      IMAGE=$OPTARG
      [[ -f $IMAGE ]] || error $IMAGE is not a file.
      ;;
    a)
      ARTIST=$OPTARG
      ;;
    t)
      TITLE=$OPTARG
      ;;
    s)
      case $OPTARG in
        line|point|p2p|cline)
          STYLE=OPTARG
          ;;
        *)
          error $STYLE is not a valid style.
      esac
      ;;
    o)
      OUT_FILE=${OPTARG}
      ;;
    *)
      usage
      exit 1
      ;;
  esac
done

# -o has highest priority; otherwise try to use both -a and -t
if [ $OUT_FILE ]; then
  OUTPUT=${OUT_FILE}.mp4
elif [ $ARTIST -a $TITLE ]; then
    OUTPUT=${ARTIST}-${TITLE}.mp4
  else
    error Output file name could not be determined.
  fi
fi

[[ $WAV ]] || error Missing required -w option.
[[ $IMAGE ]] || error Missing required -i option.

convert -resize 1280x720 "${IMAGE}" -background none -gravity center -extent 1280x720 $IMAGE_TMP

$FFMPEG -y -i "$WAV" -loop 1 -i $IMAGE_TMP -filter_complex \
  "[0:a] showwaves=s=1280x310:mode=${STYLE}:r=25,colorkey=0x000000:0.1:0.5[wave];[1:v][wave] overlay=y=H-h:eval=init[canvas];[canvas]drawtext=fontfile='Jura-Book.ttf':fontsize=60:text='${ARTIST}':x=30:y=(h-70-text_h*2.5):fontcolor=white:shadowy=2:shadowx=2:shadowcolor=black,drawtext=fontfile='Jura-Book.ttf':fontsize=72:text='${TITLE}':x=20:y=(h-70-text_h):fontcolor=ff6600:shadowy=2:shadowx=2:shadowcolor=black" -shortest -codec:v libx264 -crf 18 -preset fast -codec:a aac -strict -2 -b:a 192k $MP4_TMP

if [[ -e end.mp4 ]]; then
    $FFMPEG -y -i $MP4_TMP -i end.mp4 -filter_complex "[0:0] [0:1] [1:0] [1:1] concat=n=2:v=1:a=1 [v] [a]" -map "[v]" -map "[a]" ${OUTPUT}
else
    mv $MP4_TMP $OUTPUT
fi

rm -f $IMAGE_TMP $MP4_TMP

[[ -f $OUTPUT ]] && echo Wrote $OUTPUT || error Failed to create $OUTPUT
