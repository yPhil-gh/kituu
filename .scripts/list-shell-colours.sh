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
