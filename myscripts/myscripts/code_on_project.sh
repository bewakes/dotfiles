#!/bin/bash

read -p "Enter the project name: " projectname
dir=$(cat ~/.bro/projects/$projectname)

echo $1

if [[ "$1" == "setup" ]]; then
    echo $dir
    # Check if haskell project
    res=$(ls $dir | grep -E "^stack.yaml$|*.cabal$")
    echo $res
    if [[ $res ]]; then
        # fire up three terminals
        cd $dir && termite --class haskell-project -e "bash" &

        # Spawn one to the right
        bspc node --presel-dir east --presel-ratio 0.8 -i
        cd $dir && termite --class haskell-project -e "bash" &

        # Spawn one to the bottom
        bspc node --presel-dir south --presel-ratio 0.75 -i
        cd $dir && termite --class haskell-project -e "bash" &
        exit 0
    fi
fi

cd $dir && termite -e "bash" &
