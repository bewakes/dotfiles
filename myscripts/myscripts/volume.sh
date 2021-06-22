#!/bin/bash

muted=`amixer sget Master | grep Mono: | awk -F'[][]' '{print $6}'`
if [[ $muted == "off" ]]; then
    echo "MUTED"
else
    amixer sget Master | grep Mono: | awk -F'[][]' '{print $2}'
fi
