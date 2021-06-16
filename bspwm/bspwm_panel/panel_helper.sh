#/bin/bash

COLOR_GREEN="#90ee90"
COLOR_ORANGE="#b1a57d"
COLOR_RED="#ff0000"

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
        color=$COLOR_ORANGE
    fi
    echo $color
}
