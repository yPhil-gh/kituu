#!/bin/bash

if [ $# -eq 0 ]
then
	echo "USAGE: converts_sub.sh <filename.sub>"
	exit
fi

mv "$1" "/tmp/$1.old.$$"
iconv -f windows-1256 -t utf8 "/tmp/$1.old.$$" -o "$1" && echo "Done"