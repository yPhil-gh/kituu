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

[[ ! $(groups $USER | grep "audio") ]] && echo "### User $USER is not in the audio group, adding it." && sudo adduser $USER audio && echo "
## Your user/settings have been reloaded : Now re-run $0.
" && sudo su - $USER

SRC_DIR=~/tmp/PLUGIN_PACKS
LV2_DIR=/usr/local/lib/lv2
# LV2_DIR=~/tmp/ZLV2
LXVST_DIR=/usr/local/lib/lxvst
# LXVST_DIR=~/tmp/ZVST

[[ -d $SRC_DIR ]] && cd $SRC_DIR || mkdir -v $SRC_DIR && cd $SRC_DIR

# [[ ! -d $LV2_DIR ]] && mkdir -v $LV2_DIR
# [[ ! -d $LXVST_DIR ]] && mkdir -v $LXVST_DIR

[[ ! -w $LV2_DIR ]] && sudo chown -R .audio $LV2_DIR && sudo chmod -R g+w $LV2_DIR
[[ ! -w $LXVST_DIR ]] && sudo chown -R .audio $LXVST_DIR && sudo chmod -R g+w $LXVST_DIR

[[ ! -w $LV2_DIR ]] && echo "## No writable $LV2_DIR, exiting" && exit 1
[[ ! -w $LXVST_DIR ]] && echo "## No writable $LXVST_DIR, exiting" && exit 1

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
        cp -R $D_PLUGIN $D_DEST_DIR
    done
done
