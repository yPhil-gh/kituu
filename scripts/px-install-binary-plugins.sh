#!/bin/bash

cd ~/tmp/zz
pwd

# exit 0

strpos() {
    str=${1/$2*/}
    return `expr length $str`
}

DISTRHO="http://downloads.sourceforge.net/project/distrho/Ports/Arctican-Plugins/arctican-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/dRowAudio-Plugins/drowaudio-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/Juced-plugins/juced-plugins_linux32_20120518-2.7z
http://downloads.sourceforge.net/project/distrho/Ports/TAL-Plugins/tal-plugins_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/HighLife/highlife_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/HybridReverb2/hybridreverb2_linux32_20120518.7z
http://downloads.sourceforge.net/project/distrho/Ports/Wolpertinger/wolpertinger_linux32_20120518-2.7z"

for D_URL in $DISTRHO ; do

    echo -e "
entering $(pwd)"

    D_FILE=$(basename $D_URL)
    D_DIR=${D_FILE:0:$(( ${#D_FILE} -3 ))}

    echo "
## $D_DIR"
    # strpos $D_DIR "_linux32"
    # POS=$?
    # # D_FILE_LENGH=$((${POS} +8))

    echo "Name should be ${D_DIR:0:$(( ${POS} ))} given that pos is $POS"
    # echo ${D_DIR:0:26}

    # COOL_NAME=${D_DIR:0:$(( ${POS} ))}

    wget $D_URL
    p7zip -d $D_FILE
    cd $(ls)
    echo -e "
## ${D_DIR:0:$(( ${POS} ))} ($D_FILE) wow, i'm in $(pwd)"
    # exit 0
    # cp -R -v lv2/* /usr/local/lib/lv2/
    # cp -R -v lv2/* ~/tmp/zzz/

    cd lv2

    for LV2_PLUGIN_DIR in $(ls) ; do
        cp -R -v $LV2_PLUGIN_DIR ~/tmp/zzz/
    done

    echo -e "
leavin $(pwd)"
    cd ../..
    rm -rf $(ls)

    # # exit 0
    # echo "Yo, D_URL iz $D_URL"
    # echo "Yo, D_FILE iz $D_FILE"
    # echo "Yo, dir iz $D_DIR"
done



# http://sourceforge.net/projects/distrho/files/Ports/Arctican-Plugins/arctican-plugins_linux32_20120518.7z/download
# http://sourceforge.net/projects/distrho/files/Ports/dRowAudio-Plugins/drowaudio-plugins_linux32_20120518.7z/download
# http://sourceforge.net/projects/distrho/files/Ports/Juced-plugins/juced-plugins_linux32_20120518-2.7z/download
# http://sourceforge.net/projects/distrho/files/Ports/TAL-Plugins/tal-plugins_linux32_20120518.7z/download
# http://sourceforge.net/projects/distrho/files/Ports/HighLife/highlife_linux32_20120518.7z/download
# http://sourceforge.net/projects/distrho/files/Ports/HybridReverb2/hybridreverb2_linux32_20120518.7z/download
# http://sourceforge.net/projects/distrho/files/Ports/Wolpertinger/wolpertinger_linux32_20120518-2.7z/download
