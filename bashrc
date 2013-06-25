case `uname -s` in
	Darwin)
		OS=osx
		;;
	Linux)
		OS=linux
		;;
	*)
		OS=unknown
		;;
esac

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
dumb)
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    ;;
*)
    PS1='${debian_chroot:+($debian_chroot)}\[\033[34m\]\u@\h:\[\033[00m\]\[\033[31m\]\w\$\[\033[00m\] '
    ;;
esac

# Comment in the above and uncomment this below for a color prompt
#PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

alias findn='find . -name'

# pico suxx
export EDITOR=vi
export VISUAL=vi


# --- PATH -------------------------------------------------------------------
# ----------------------------------------------------------------------------

function conspath() {
    for d in $@; do
        if [ -d $d ]; then
            export PATH="$1:$PATH"
#	else
#	    echo '"' $d '" is not a directory'
        fi
    done
}

# gcc arm cross-compiler
conspath "$HOME/arm-cs-tools/bin"

# yes, I want to use the big guns
conspath /sbin /usr/sbin /usr/local/sbin

# my stuff
conspath "$HOME/bin"



# --- MACHINE LOCAL ----------------------------------------------------------
# ----------------------------------------------------------------------------

if [ -r ~/.bashrc_local ]; then
   . ~/.bashrc_local
fi 


defaults write ~/.MacOSX/environment PATH "$PATH"
