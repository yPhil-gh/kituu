#!/bin/bash

DISTRHO="http://downloads.sourceforge.net/project/distrho/Ports/Arctican-Plugins/arctican-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/dRowAudio-Plugins/drowaudio-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/Juced-plugins/juced-plugins_linux32_20120518-2.7z
http://downloads.sourceforge.net/project/distrho/Ports/TAL-Plugins/tal-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/HighLife/highlife_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/HybridReverb2/hybridreverb2_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/Wolpertinger/wolpertinger_linux32_20120518-2.7z"

SOURCE_DIR=~/tmp/zz
# DEST_DIR=/usr/local/lib/lv2/
DEST_DIR=~/tmp/zzz

cd $SOURCE_DIR

strpos() {
    str=${1/$2*/}
    return `expr length $str`
}

for D_URL in $DISTRHO ; do

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
    rm -rf $(ls)
done
