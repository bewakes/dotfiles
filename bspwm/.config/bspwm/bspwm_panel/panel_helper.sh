#/bin/bash

BASE=~/.config/bspwm/bspwm_panel

. $BASE/panel_colors

_get_level_color () {
    val=$1
    min=$2
    mid=$3
    op=">"

    if (( $min > $mid )); then
        t=$mid
        mid=$min
        min=$t
        op="<"
    fi

    color=$COLOR_GREEN
    if (( $(echo "$val $op $min" |bc -l) )); then
        color=$COLOR_RED
    elif (( $(echo "$val $op $mid" |bc -l) )); then
        color=$COLOR_LIGHT_YELLOW
    fi
    echo $color
}
