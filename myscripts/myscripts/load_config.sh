#!/bin/bash

config_root=~/.config

case $1 in
    xmon*)
        echo xmonad
        termite -e "nvim $config_root/xmonad/xmonad.hs"
        ;;
    nvim|vim)
        echo nvim
        termite -e "nvim $config_root/nvim/init.lua"
        ;;
    xmob*)
        echo xmobar
        termite -e "nvim $config_root/xmobar/xmobarrc"
        ;;
    *)
        echo nothing
        ;;
esac
