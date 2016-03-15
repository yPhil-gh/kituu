#!/usr/bin/env bash

selection=$(zenity --list --multiple --checklist --hide-column=4 --print-column=4 --separator="|" --title="LV2 Plugins" --column=Go --column=Type --column=Name --column=URI < \
    <(for uri in $(lv2ls); do
        PLUGIN=$(lv2info $uri | head -n +4 | tail -n +3 | paste -sd "|" - | sed 's/Name://g;s/Class://g')
        echo "Go"
        echo ${PLUGIN#*|}
        echo ${PLUGIN%|*}
        echo ${uri}
        done))

IFS='|' read -ra URI <<< "$selection"
for i in "${URI[@]}"; do
    if [[ $i == http* ]] || [[ $i == urn* ]] ; then
        jalv.gtkmm --name=${i##*/} ${i} &
    fi
done
