#!/bin/bash

read -p "Enter the project name: " projectname
dir=$(cat ~/.bro/projects/$projectname)

if [[ -z $1 ]]; then
    exit 0
fi
if [[ "$1" == "" ]]; then
    exit 0
fi

if [[ "$1" == "setup" ]]; then
    # Check if haskell project
    res=$(ls $dir | grep -E "^stack.yaml$|*.cabal$")
    echo $res
    if [[ $res ]]; then
        # fire up three terminals
        cd $dir && termite --class haskell-nvim -e "bash" &
        sleep 0.5s

        # Create a receptacle on the right
        bspc node --presel-dir east --presel-ratio 0.8 -i
        # Create a receptacle on the bottom
        bspc node --presel-dir south --presel-ratio 0.75 -i

        # Spawn a terminal
        cd $dir && termite --class haskell-stack -e "bash" &
        sleep 0.5s
        # Move the terminal to a receptacle
        bspc node $(bspc query -N -n focused) -n $(bspc query -N -n .leaf.\!window | head -n 1)

        # Spawn another
        cd $dir && termite --class haskell-repl -e "bash" &
        sleep 0.5s
        bspc node $(bspc query -N -n focused) -n $(bspc query -N -n .leaf.\!window)

        # Move to receptacle
        exit 0
    fi
elif [[ "$1" == "just-terminal" ]]; then
    cd $dir && termite -e "bash" &
fi

