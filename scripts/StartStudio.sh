#!/bin/bash

#VST_PATH=/usr/lib/vst:/usr/local/lib/vst
#export VST_PATH

if [ "$1" == "-k" ] ; then
    killall qjackctl qtractor vmpk qmidinet jackd
else
    killall qjackctl qtractor vmpk qmidinet jackd
    sleep 1
    qjackctl &
    sleep 2
    qmidinet &
    # sleep 1
    # ams -N Guitar -l ~/Documents/Music/00-PRESETS/MyGrt00.ams &
    # sleep 1
    # ams -N BassSynth -l ~/Documents/Music/00-PRESETS/MySynth000.ams &

    if [ "$1" == "-d" ] ; then
        /usr/local/bin/qtractor $2 &
    else
        qtractor $1 &
    fi
   vmpk &
fi
