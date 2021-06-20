#!/usr/bin/bash

if [ -z $1 ]; then
	echo "Usage: $0 <name of hidden scratchpad window> <command to run in termite>"
	exit 1
fi

cmd=$2

pids=$(xdotool search --class ${1})
echo pids $pids .
if [[ -z $pids ]]; then
    termite --class $1 -e "$cmd" &
    pids=$!
fi

for pid in $pids; do
	bspc node $pid --flag hidden -f
done
