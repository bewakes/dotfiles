#!/bin/bash

read -p "Enter the project name: " projectname

if [[ -z "$projectname" || "$projectname" == "" ]]; then
    echo no projectname
    exit 0
fi

dir=$(cat ~/.bro/projects/$projectname)

if [[ "$1" == "setup" ]]; then
    # Check if haskell project
    is_haskell=$(ls $dir | grep -E "^stack.yaml$|*.cabal$")
    if [[ $is_haskell ]]; then
        # fire up three terminals
        cd $dir && termite --class haskell-nvim &
        sleep 0.5s

        # Create a receptacle on the right
        bspc node --presel-dir east --presel-ratio 0.8 -i
        # Create a receptacle on the bottom
        bspc node --presel-dir south --presel-ratio 0.75 -i

        # Spawn a terminal
        cd $dir && termite --class haskell-stack &
        sleep 0.5s
        # Move the terminal to a receptacle
        bspc node $(bspc query -N -n focused) -n $(bspc query -N -n .leaf.\!window | head -n 1)

        # Spawn another
        cd $dir && termite --class haskell-repl &
        sleep 0.5s

        # Move to receptacle
        bspc node $(bspc query -N -n focused) -n $(bspc query -N -n .leaf.\!window)
    else
        cd $dir && termite &
    fi
elif [[ "$1" == "" ]]; then
    echo no action so just spawning terminal
    cd $dir && termite &
fi

