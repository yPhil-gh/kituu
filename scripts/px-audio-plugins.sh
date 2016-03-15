#!/usr/bin/env bash

selection=$(zenity --list --multiple --checklist --hide-column=4 --print-column=4 --separator="|" --title="LV2 Plugins" --column=Go --column=Class --column=Name --column=URI < \
    <(for URI in $(lv2ls); do
        PLUGIN=$(lv2info $URI | head -n +4 | tail -n +3 | paste -sd "|" - | sed 's/Name://g;s/Class://g' | xargs)
        echo -e "Go \n ${PLUGIN#*|} \n ${PLUGIN%|*} \n ${URI}"
        done))

IFS='|' read -ra URIs <<< "$selection"
for i in "${URIs[@]}"; do
    jalv.gtkmm --name=${i##*/} ${i} &
done
