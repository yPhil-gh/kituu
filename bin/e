#!/bin/sh

if [ "$(pidof emacs)" ] ; then
    emacsclient "$@" &
else
    emacs -mm --no-bitmap-icon "$@" &
fi
