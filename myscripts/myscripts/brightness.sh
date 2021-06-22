#!/usr/bin/sh

dir=/sys/class/backlight/intel_backlight/
file="$dir"brightness
maxfile="$dir"max_brightness
brightness=`cat $file`
if [ $1 == "inc" ]
then
	let sum=$brightness+$2
elif [ $1 == "dec" ]
then
	let sum=$brightness-$2
fi

brightness=$sum
if [ $brightness -ge `cat $maxfile` ]
then
	brightness=`cat $maxfile`
else
	temp=
fi
echo $brightness > $file
