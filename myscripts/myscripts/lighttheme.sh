#!/bin/bash

# vim_config=~/.config/nvim/init.lua

# for alacritty
# alacritty-colorscheme -a solarized_light.yaml

# for termite
termite-themes --switch-to base16-solarized-light

# cat $vim_config | sed "s/vim.o.background = 'dark'/vim.o.background = 'light'/g" > /tmp/_vimrc
# cat /tmp/_vimrc > $vim_config
