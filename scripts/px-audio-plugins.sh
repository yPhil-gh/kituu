#!/usr/bin/env bash

selection=$(zenity --list --multiple --checklist --hide-column=4 --print-column=4 --separator="|" --title="LV2 Plugins" --column=Go --column=Type --column=Name --column=URI < \
    <(for plugin in $(lv2ls); do
    NAME=$(lv2info "$plugin" | grep 'Name:' | grep -v http | awk '{print $0;exit}')
    CLASS=$(lv2info "$plugin" | grep 'Class:' | grep -v http | awk '{print $2;exit}')
    echo "Go"
    echo ${CLASS}
    echo ${NAME#*:}
    echo ${plugin}
done))

IFS='|' read -ra URI <<< "$selection"
for i in "${URI[@]}"; do
    if [[ $i == http* ]] || [[ $i == urn* ]] ; then
        jalv.gtkmm --name=${i##*/} ${i} &
    fi
done
