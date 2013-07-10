#!/bin/bash

#VST_PATH=/usr/lib/vst:/usr/local/lib/vst
#export VST_PATH

apps="killall qjackctl vmpk qmidinet jackd"

if [ "$1" == "-k" ] ; then
    killall $apps ardour-3.2
else
    killall -w $apps
    sleep 1
    qjackctl &
    sleep 2
    qmidinet &
    # sleep 1
    # ams -N Guitar -l ~/Documents/Music/00-PRESETS/MyGrt00.ams &
    # sleep 1
    # ams -N BassSynth -l ~/Documents/Music/00-PRESETS/MySynth000.ams &

    ardour3 >nul 2>&1 &

    # if [ "$1" == "-d" ] ; then
    #     /usr/local/bin/qtractor -style -plastique $2 &
    # else
    #     qtractor -style -plastique $1 &
    # fi
   vmpk &
fi
