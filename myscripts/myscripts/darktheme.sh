#!/bin/bash

# vim_config=~/.config/nvim/init.lua

# for alacritty
# alacritty-colorscheme -a gruvbox_dark.yaml

# for termite
termite-themes --switch-to base16-gruvbox-dark-hard

# cat $vim_config | sed "s/vim.o.background = 'light'/vim.o.background = 'dark'/g" > /tmp/_vimrc
# cat /tmp/_vimrc > $vim_config
