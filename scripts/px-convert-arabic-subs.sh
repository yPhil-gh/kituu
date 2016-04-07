#!/bin/bash

[[ $# -eq 0 ]] && echo "USAGE: $0 subtitle_file" ; exit

mv "$1" "/tmp/$1.old.$$"
iconv -f windows-1256 -t utf8 "/tmp/$1.old.$$" -o "$1" && echo "Done"
