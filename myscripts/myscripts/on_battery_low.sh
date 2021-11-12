#!/bin/bash

is_charging=$(acpi -b | grep Discharging)
percent=$(acpi -b | awk 'BEGIN {FS=", "} {print $2}' | sed 's/%//g')

if [[ "$is_charging" == "" ]]; then
    # Check if almost full charged
    if [[ $percent -gt 97 ]]; then
        notify-send 'Battery' 'Almost Full!!'
        paplay /usr/share/sounds/freedesktop/stereo/suspend-error.oga
    fi
    exit 0
fi

if [[ $percent -lt 15 ]]; then
    notify-send 'Battery' 'Low!!'
    paplay /usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga
    a=2
fi


