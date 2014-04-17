#!/bin/bash

#VST_PATH=/usr/lib/vst:/usr/local/lib/vst
#export VST_PATH

# hydrogen
# yoshimi
# vmpk
# qmidinet

APPS="
qjackctl
"
DAW=$1

ALL_APPS="$APPS"

kill_all() {
    for APP in $ALL_APPS ; do
        if [[ $(pidof $APP) ]] ; then
            echo  "## Killing $APP ($(pidof $APP))"
            killall $APP
        fi
    done
}

if [ "$2" == "-k" ] ; then
    kill_all
    killall jackd
    exit 0
fi

if [ $DAW == "radium" ] ; then
    echo "yo"
    cd ~/src/radium/bin/
    ./radium
fi

kill_all

# [[ $(ps aux|grep a2jmidi) ]] && echo "## aj2midi is running" || a2jmidi -u


for APP in $APPS ; do
    $APP&
    echo "## Starting $APP ($(pidof $APP))"
    sleep 2
done

$DAW $2&
echo "## Starting $DAW ($(pidof $APP))"
