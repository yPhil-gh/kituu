#!/bin/sh

infile=$1
tmpfile="$1-tmp.mp4"
outfile="$1-new.mp4"
options="-vcodec libx264 -b 512k -flags +loop+mv4 -cmp 256 \
	   -partitions +parti4x4+parti8x8+partp4x4+partp8x8+partb8x8 \
	   -me_method hex -subq 7 -trellis 1 -refs 5 -bf 3 \
	   -flags2 +bpyramid+wpred+mixed_refs+dct8x8 -coder 1 -me_range 16 \
           -g 250 -keyint_min 25 -sc_threshold 40 -i_qfactor 0.71 -qmin 10\
	   -qmax 51 -qdiff 4"

# Half size
options=$options" -vf scale=iw*0.5:-1"
# Copy audio
options=$options" -codec:a copy"
# Shut up
options=$options" -loglevel info "

# echo "Options : $options"

avconv -y -i "$infile" -threads auto $options "$outfile"
# avconv -y -i "$infile" -an -pass 1 -threads auto $options "$tmpfile"

# avconv -y -i "$infile" -acodec aac -strict experimental -ar 44100 -ab 96k -pass 2 -threads auto $options "$tmpfile"

# qt-faststart "$tmpfile" "$outfile"
