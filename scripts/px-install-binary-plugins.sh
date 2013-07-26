#!/bin/bash

PLUGIN_PACKS="http://downloads.sourceforge.net/project/distrho/Ports/HighLife/highlife_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/Arctican-Plugins/arctican-plugins_linux32_20120518.7z
http://www.mucoder.net/en/hypercyclic/v0101/download/latest/hypercyclic.1.1.367.linux.zip
http://downloads.sourceforge.net/project/distrho/Ports/dRowAudio-Plugins/drowaudio-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/Juced-plugins/juced-plugins_linux32_20120518-2.7z
http://downloads.sourceforge.net/project/distrho/Ports/TAL-Plugins/tal-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/HybridReverb2/hybridreverb2_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/Wolpertinger/wolpertinger_linux32_20120518-2.7z"

[[ ! $(groups $USER | grep "audio") ]] && echo "User $USER is not in the audio group, adding it." && sudo adduser $USER audio && echo "
#### Your user/settings have been reloaded : Now re-run $0.
" && sudo su - $USER

# exit 0

# sudo su - $USER

# http://www.mucoder.net/en/hypercyclic/v0101/download/latest/hypercyclic.1.1.367.linux.zip
# https://dl.dropbox.com/u/4063301/ThrillseekerXTC_1.0.1.zip

SRC_DIR=~/tmp/PLUGIN_PACKS
# LV2_DIR=/usr/local/lib/lv2/
LV2_DIR=~/tmp/ZLV2
LXVST_DIR=~/tmp/ZVST

[[ -d $SRC_DIR ]] && cd $SRC_DIR || mkdir -v $SRC_DIR && cd $SRC_DIR

[[ ! -d $LV2_DIR ]] && mkdir -v $LV2_DIR
[[ ! -d $LXVST_DIR ]] && mkdir -v $LXVST_DIR

[[ ! -w $LV2_DIR ]] && sudo chown -R .audio $LV2_DIR && sudo chmod -R g+w $LV2_DIR
[[ ! -w $LXVST_DIR ]] && sudo chown -R .audio $LXVST_DIR && sudo chmod -R g+w $LXVST_DIR

# Now just
# sudo su - $USER && $0

for D_URL in $PLUGIN_PACKS ; do
    D_FILE=$(basename $D_URL)

    echo -e "
## ${D_FILE}"

    wget -nv $D_URL && 7z x $D_FILE
    PLUGIN_LV2=$(find . -name "*.lv2")
    PLUGIN_VST=$(find . -name "*.so")

    if [[ $PLUGIN_LV2 ]] ; then
        PLUGIN_VST=""
        D_DEST_DIR=$LV2_DIR
        # echo "
#### kayn LV2! ($PLUGIN_LV2) in $D_DEST_DIR" ;
    else
        if [[ $PLUGIN_VST && ! $PLUGIN_LV2 ]] ; then
        D_DEST_DIR=$LXVST_DIR
            # echo "
#### kayn VST! ($PLUGIN_VST) in $D_DEST_DIR" ;
        fi
    fi

    ALL_PLUGINS="$PLUGIN_VST $PLUGIN_LV2"

    echo "
### All plugz : $ALL_PLUGINS
"

    for D_PLUGIN in $ALL_PLUGINS ; do
        echo "
### Copying $D_PLUGIN to $D_DEST_DIR"
        cp -R $D_PLUGIN $D_DEST_DIR
    done

    # for LV2_PLUGIN in $PLUGIN_LV2 ; do
    #     echo "Copying $LV2_PLUGIN to $LV2_DIR"
    #     cp -R $LV2_PLUGIN $LV2_DIR
    # done

    # echo -e "## Copied $LV2_PLUGIN to $LV2_DIR"
    rm -rf *
done
