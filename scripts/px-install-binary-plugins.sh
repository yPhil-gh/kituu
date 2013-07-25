#!/bin/bash

DISTRHO="http://downloads.sourceforge.net/project/distrho/Ports/Arctican-Plugins/arctican-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/dRowAudio-Plugins/drowaudio-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/Juced-plugins/juced-plugins_linux32_20120518-2.7z
http://downloads.sourceforge.net/project/distrho/Ports/TAL-Plugins/tal-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/HighLife/highlife_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/HybridReverb2/hybridreverb2_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/Wolpertinger/wolpertinger_linux32_20120518-2.7z"

# http://www.mucoder.net/en/hypercyclic/v0101/download/latest/hypercyclic.1.1.367.linux.zip
# https://dl.dropbox.com/u/4063301/ThrillseekerXTC_1.0.1.zip

SRC_DIR=~/tmp/DISTRHO
DEST_DIR=/usr/local/lib/lv2/
# DEST_DIR=~/tmp/zzz

[[ -d $SRC_DIR ]] && cd $SRC_DIR || mkdir -v $SRC_DIR

[[ ! -w $DEST_DIR ]] && sudo adduser $USER audio && sudo chown -R .audio $DEST_DIR && sudo chmod -R g+w $DEST_DIR

# Now just
# sudo su - $USER && $0

# pwd && echo "plop"
# exit 0

strpos() {
    str=${1/$2*/}
    return `expr length $str`
}

for D_URL in $DISTRHO ; do
    cd $SRC_DIR
    D_FILE=$(basename $D_URL)
    D_DIR=${D_FILE:0:$(( ${#D_FILE} -3 ))}

    strpos $D_FILE "_linux32"
    POS=$?

    echo -e "
## ${D_DIR:0:$(( ${POS} ))}"

    wget -nv $D_URL && p7zip -d $D_FILE && cd "$(ls)/lv2"

    for LV2_PLUGIN_DIR in $(ls) ; do
        cp -R $LV2_PLUGIN_DIR $DEST_DIR
    done

    echo -e "## Copied $(pwd) to $DEST_DIR"
    cd ../..
    rm -rfi $(ls)
done
