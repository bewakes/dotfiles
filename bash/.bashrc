# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

export WINIT_HIDPI_FACTOR=1.5

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000 HISTFILESIZE=2000 
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

# if [ "$color_prompt" = yes ]; then
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;31m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
#else
#    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
#fi

unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto' 
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
# alias la='ls -A'
alias l='ls -CF'

alias search='./pratices/shell/bin/search'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
elif [ -f /opt/homebrew/etc/bash_completion.d/git-completion.bash ]; then
  . /opt/homebrew/etc/bash_completion.d/git-completion.bash
fi

export PATH="$PATH:$HOME/practices/shell/bin/:/home/bibek/.local/bin"
export PATH="$PATH:$HOME/myscripts"
export PATH="/usr/local/texlive/2022/bin/universal-darwin:$PATH"

export PATH=$PATH:/home/bibek/.yarn/bin/
export PATH=$PATH:/usr/bin/
export PATH=$PATH:/home/bibek/.gem/ruby/2.5.0/bin

record_command() {
    # make dir .logs/commands/
    logdir=$HOME"/.logs/commands/"
    filename=`date +"%Y-%B-%d"`".log"
    mkdir -p $logdir
    # get time
    tm=`date +"%H:%M:%S"`
    # get the command
    tmp=`history 1`
    ttmp=`echo $tmp | sed 's/[0-9]* //'`
    if [[ $ttmp == *"shutdown"* ]]
    then
        return
    fi
    filepath=$fullpath$file
    echo $tm $ttmp >> $logdir$filename
}

export PROMPT_COMMAND='record_command'

alias checkmic="ffmpeg -f alsa -i hw:1 -t 10 output.wav"

### Bro
export BRO_STATION=/home/bibek/.bro
export WORKSTATION=/home/bibek/projects
# source ~/.bro/activate

# alias for onnecting to taskfore
alias ssh="TERM=rxvt ssh"

alias ls='ls --color'

export FZF_DEFAULT_COMMAND="rg --files"

# source ~/.fzf.bash

PS1="\[\e[1m\e[95m\]\$(show-branch)\[\e[0m\]$PS1"

export ASSISTANT_DIR=~/projects/assistant/

export TERM=screen-256color

EDITOR=nvim
# search-logs() {
#     grep "$1" ~/.logs/commands -r | sort | awk '{$1=""; print $0}'
# }
# set -o vi
bind -m vi-command 'Control-l: clear-screen'
bind -m vi-insert 'Control-l: clear-screen'

# ports
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
export MANPATH=/opt/local/share/man:$MANPATH

# MACBOOK: Intel Platform
alias intel="arch -x86_64"

alias bc="bitcoin-cli -regtest"

. "$HOME/.cargo/env"
export PATH="$HOME/.cargo/bin:$PATH"
eval export HOMEBREW_PREFIX="/opt/homebrew";
export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
export HOMEBREW_REPOSITORY="/opt/homebrew";
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin${PATH+:$PATH}";
export MANPATH="/opt/homebrew/share/man${MANPATH+:$MANPATH}:";
export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH="$PATH:/Users/bibek/.foundry/bin"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH=$BUN_INSTALL/bin:$PATH

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH=$BUN_INSTALL/bin:$PATH
PATH="/opt/homebrew/opt/gawk/libexec/gnubin:$PATH"
export DEVKITPRO=/opt/devkitpro
export DEVKITARM=/opt/devkitpro/devkitARM

[ -f "/Users/bibek/.ghcup/env" ] && . "/Users/bibek/.ghcup/env" # ghcup-env

### Bro
export BRO_STATION=/Users/bibek/.bro
export WORKSTATION=/Users/bibek/projects
source /Users/bibek/.bro/activate

# pnpm
export PNPM_HOME="/Users/bibek/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
