#!/usr/bin/env bash

JPG=`ls -1 *.JPG 2>/dev/null | wc -l`
jpg=`ls -1 *.jpg 2>/dev/null | wc -l`

if [[ $JPG != 0  ]] ; then
    echo "############### CONVERT"

    for IMG in *.JPG ; do
        convert -interlace Plane -gaussian-blur 0.05 -quality 25% "${IMG}" "${IMG%.*}.jpg"
        echo "Converting ${IMG} to ${IMG%.*}.jpg"
        git add -v "${IMG%.*}.jpg"
        rm -fv "${IMG}"
    done
    echo "############### CONVERT DONE"
else
    echo "############### NOTHING TO CONVERT"
fi


if [[ $jpg != 0  ]] ; then

    jsfile='images.js'
    iter=0
    number_of_files=$(ls *.jpg | wc -l)

    echo "############### ADD"

    echo -e "var data = Array(" > $jsfile

    for img in *.jpg ; do
        id=$(identify -verbose $img | grep -E 'exif:ExposureTime|exif:\FocalLength\b|exif:FocalLengthIn35mmFilm|exif:MaxApertureValue|exif:\Flash\b|exif:ISOSpeedRatings|exif:\Make\b|exif:Model|exif:DateTimeOriginal' | sed "s/exif:/ /g")


        let "iter++"
        echo "Including file $iter (${img%.*}.jpg) in $jsfile"

        read a b c d e f g h i j k l m n o p q r s t <<< ${id}
        DateTimeOriginal=$b" @ "$c
        Time=$c
        ExposureTime=$e
        Flash=$g
        FocalLength=$i
        FocalLengthIn35mmFilm=$k
        ISOSpeedRatings=$m
        Make=$o
        MaxApertureValue=$q
        Model=$s
        Camera=$o" "$s

        echo -e "'${img%.*}.jpg!$DateTimeOriginal | Exposure / Aperture: $ExposureTime | $MaxApertureValue | Flash GN: $Flash | FocalLength: $FocalLength mm (135 eqv: $FocalLengthIn35mmFilm mm) | $ISOSpeedRatings ISO | $Camera'" >> $jsfile

        [[ ! "$iter" -eq "$number_of_files" ]] && echo -e "," >> $jsfile


        convert -interlace Plane -gaussian-blur 0.05 -quality 25% "${img}" "${img%.*}.jpg"
        # convert -font helvetica -fill blue -pointsize 158 -draw "text 20,60 'plop!'" "${i}" "${i%.*}.jpg"
        # convert -font helvetica -fill red -pointsize 48 -draw "text 15,50 'plop!'" -interlace Plane -gaussian-blur 0.05 -quality 25% "${i}" "${i%.*}.jpg"
    done

    echo -e ");" >> $jsfile
    echo "############### ADD DONE"
else
    echo "############### NOTHING TO ADD"
fi
