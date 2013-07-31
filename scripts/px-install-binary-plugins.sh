#!/bin/bash

PLUGIN_PACKS="http://downloads.sourceforge.net/project/distrho/Ports/HighLife/highlife_linux32_20120518.7z
http://www.extentofthejam.com/DigitsVST-Linux-1.3.tar.gz
https://sites.google.com/site/ccernnaudio/vst-plugins/backup.zip
http://downloads.sourceforge.net/project/distrho/Ports/Arctican-Plugins/arctican-plugins_linux32_20120518.7z
http://www.mucoder.net/en/hypercyclic/v0101/download/latest/hypercyclic.1.1.367.linux.zip
http://downloads.sourceforge.net/project/distrho/Ports/dRowAudio-Plugins/drowaudio-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/Juced-plugins/juced-plugins_linux32_20120518-2.7z
http://downloads.sourceforge.net/project/distrho/Ports/TAL-Plugins/tal-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/HybridReverb2/hybridreverb2_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/Wolpertinger/wolpertinger_linux32_20120518-2.7z"

SRC_DIR=~/src/PLUGIN_PACKS
LV2_DIR=/usr/local/lib/lv2
LXVST_DIR=/usr/local/lib/lxvst

[[ -d $SRC_DIR ]] && cd $SRC_DIR || mkdir -v $SRC_DIR && cd $SRC_DIR

for D_URL in $PLUGIN_PACKS ; do

    rm -rf *

    D_FILE=$(basename $D_URL)
    D_URI=${D_URL:0:$( expr ${#D_URL} - ${#D_FILE} )}
    D_FILE_TGZ=$(echo "$D_FILE" | grep "tar.gz" )
    [[ $D_FILE_TGZ ]] && EXT_COMMAND="tar -xzf " || EXT_COMMAND="7z x "

    echo -e "
## Downloading ${D_FILE} (from $D_URI)"

    wget -q --secure-protocol=auto $D_URL && echo "### Downloaded $D_FILE in $SRC_DIR" && $EXT_COMMAND $D_FILE > /dev/null
    PLUGIN_LV2=$(find . -name "*.lv2")
    PLUGIN_VST=$(find . -name "*.so")

    if [[ $PLUGIN_LV2 ]] ; then
        PLUGIN_VST=""
        D_DEST_DIR=$LV2_DIR
    else
        if [[ $PLUGIN_VST && ! $PLUGIN_LV2 ]] ; then
        D_DEST_DIR=$LXVST_DIR
        fi
    fi

    ALL_PLUGINS="$PLUGIN_VST $PLUGIN_LV2"

    for D_PLUGIN in $ALL_PLUGINS ; do
        echo "### Copying $D_PLUGIN to $D_DEST_DIR"
        sudo cp -R $D_PLUGIN $D_DEST_DIR
    done
done
