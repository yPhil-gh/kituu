#!/bin/bash

prefix="\[\e["
suffix="\]"
for i in {1..255}
do
	mycolor="\e[0;38;5;${i}m"
	mycolorname="0;38;5;${i}m"
	nocolor="\e[0m"
	echo -ne "This "${mycolor}"color"${nocolor}
	echo " name is = ""\""${prefix}${mycolorname}${suffix}"\""
done

t='gYw'

printf '            '
printf '%8s' {40..47}m
printf '\n'

fg_colors=({,1}m)
for n in {30..37}m; do
  fg_colors+=({,1\;}"$n")
done

for fgc in "${fg_colors[@]}"; do
  printf " %5s \033[%s  $t  " "$fgc" "$fgc"
  for bgc in {40..47}m; do
    printf " \033[%s\033[%s  $t  \033[0m" "$fgc" "$bgc"
  done
  printf '\n'
done
