# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

export PATH=$PATH:/home/janaki/.local/bin:/opt/ghc/bin:/opt/apache-activemq-5.14.5/bin

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

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi


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
alias la='ls -A'
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
fi


### Bro
export WORKSTATION=/home/janaki/projects
export BRO_STATION=/home/janaki/.bro
source /home/janaki/.bro/activate

export AWS_ACCESS_KEY_ID="12121"
export AWS_SECRET_ACCESS_KEY="31311"

export NVIM_HOME=/home/janaki/.vim/bundle/nvim

export NVM_DIR="/home/janaki/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

alias "hyperterm"="~/hyper/dist/linux-unpacked/hyper &"
alias "untrap"="trap '' DEBUG"
alias "startcompton"="compton --config ~/.compton.conf -b"

LAST_COMMAND=''
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
    LAST_COMMAND=$ttmp
    if [[ $ttmp == *"shutdown"* ]] 
    then
        return
    fi
    filepath=$fullpath$file
    echo $tm $ttmp >> $logdir$filename
}

request_ip() {
    sudo ifconfig enp3s0 $1 netmask 255.255.0.0
}

export PROMPT_COMMAND='record_command'

### Bro
export BRO_STATION=/home/janaki/.bro
export WORKSTATION=/home/janaki/projects
source /home/janaki/.bro/activate

export API_KEY='AIzaSyDXvdF3vaZ2H3u5dS7uMqVeySKG78KANKsgUDU'
export ASSISTANT_DIR=/home/janaki/dotdot/assistant/
alias assistant='bash "$ASSISTANT_DIR"assistant.sh'
export PATH="$PATH:/opt/mssql-tools/bin"

#alias up="eval `history | tail -n 1 | cut -d' ' -f 3-`"
up() {
    if [[ $LAST_COMMAND == "up" ]]; then
        cmd=`history 2 | head -n 1`
        cmd=`echo $cmd | sed 's/[0-9]* //'`
        LAST_COMMAND=$cmd
    fi
    $LAST_COMMAND
}
alias pingle='ping google.com'
SMS_IDENTITY=12345

search() {
    echo "$1" ./ -r --include=*.$2 --exclude-dir=.env
    grep "$1" ./ -r --include=*.$2 --exclude-dir=.env
}
