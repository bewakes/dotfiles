if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    source ~/.bashrc
    exec startx;
fi

# Added by serverless binary installer
export PATH="$HOME/.serverless/bin:$PATH"
if [ -e /home/bibek/.nix-profile/etc/profile.d/nix.sh ]; then . /home/bibek/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
export PATH="/home/bibek/.ebcli-virtual-env/executables:$PATH"
