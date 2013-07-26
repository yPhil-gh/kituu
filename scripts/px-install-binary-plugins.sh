#!/bin/bash

LV2_PACKS="http://downloads.sourceforge.net/project/distrho/Ports/Arctican-Plugins/arctican-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/dRowAudio-Plugins/drowaudio-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/Juced-plugins/juced-plugins_linux32_20120518-2.7z
http://downloads.sourceforge.net/project/distrho/Ports/TAL-Plugins/tal-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/HighLife/highlife_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/HybridReverb2/hybridreverb2_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/Wolpertinger/wolpertinger_linux32_20120518-2.7z"


VST_PACKS="http://www.mucoder.net/en/hypercyclic/v0101/download/latest/hypercyclic.1.1.367.linux.zip"


# http://www.mucoder.net/en/hypercyclic/v0101/download/latest/hypercyclic.1.1.367.linux.zip
# https://dl.dropbox.com/u/4063301/ThrillseekerXTC_1.0.1.zip

SRC_DIR=~/tmp/LV2_PACKS
# DEST_DIR=/usr/local/lib/lv2/
DEST_DIR=~/tmp/Z

[[ -d $SRC_DIR ]] && cd $SRC_DIR || mkdir -v $SRC_DIR && cd $SRC_DIR


[[ ! -w $DEST_DIR ]] && sudo adduser $USER audio && sudo chown -R .audio $DEST_DIR && sudo chmod -R g+w $DEST_DIR

# Now just
# sudo su - $USER && $0

strpos() {
    str=${1/$2*/}
    return `expr length $str`
}

for D_URL in $LV2_PACKS ; do
    D_FILE=$(basename $D_URL)

    echo -e "
## ${D_FILE}"

    wget -nv $D_URL && 7z x $D_FILE
    PLUGIN_ENTITY=$(find . -name "*.lv2")
    echo $PLUGIN_ENTITY
    for LV2_PLUGIN in $PLUGIN_ENTITY ; do
        echo "cp -R $LV2_PLUGIN $DEST_DIR"
        cp -R $LV2_PLUGIN $DEST_DIR
    done

    echo -e "## Copied $LV2_PLUGIN to $DEST_DIR"
    rm -rfi *
done
