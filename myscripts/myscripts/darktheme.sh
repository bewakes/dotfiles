#!/bin/bash

vim_color_config=~/.config/nvim/lua/colorscheme.lua

# for alacritty
# alacritty-colorscheme -V apply base16-gruvbox-dark-hard.yml

# for termite
# termite-themes --switch-to base16-gruvbox-dark-hard

sed -i '' 's/background=.*"/background=dark"/g' $vim_color_config  # the '' after -i is a fix for macos. don't know why
kitty +kitten themes --reload-in=all 'Everforest Dark Hard'
