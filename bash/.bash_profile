source ~/.bashrc

export BASH_SILENCE_DEPRECATION_WARNING=1

# Added by serverless binary installer
export PATH="$HOME/.serverless/bin:$PATH"
if [ -e /home/bibek/.nix-profile/etc/profile.d/nix.sh ]; then . /home/bibek/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
export PATH="/home/bibek/.ebcli-virtual-env/executables:$PATH"
. "$HOME/.cargo/env"
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"

export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
