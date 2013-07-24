#!/bin/bash

#VST_PATH=/usr/lib/vst:/usr/local/lib/vst
#export VST_PATH

APPS="
/usr/lib/qjackctl/qjackctl.real
vmpk
qmidinet
qtractor
"

kill_all() {
    for APP in $APPS ; do
        if [[ $(pidof $APP) ]] ; then
            echo  "## Killing $APP ($(pidof $APP))"
            killall $APP
        fi
    done
}

if [ "$1" == "-k" ] ; then
    kill_all
else
    kill_all

    for APP in $APPS ; do
        $APP&
        echo "## Starting $APP ($(pidof $APP))"
        sleep 1
    done
fi
