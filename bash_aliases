if [ $OS == osx ]; then
	alias ls='ls -G'
elif [ $OS == linux ]; then
	alias ls='ls --color'
fi

alias thumbdis='objdump -S -M reg-names-std --disassembler-options=force-thumb -d \!:*';

alias cdsrc='cd ~/src'

# git
alias gcl='git clone'
alias gco='git checkout'
alias gst='git status'
alias gst='git status'
alias gbr='git branch'

# todo.txt
alias todo='todo.sh'
alias dothis='todo add'
alias todone='todo do'
alias lstodo='todo ls'
alias pri='todo pri'
alias st='todo ls @synapse'
alias ht='todo ls @home'
alias ct='todo ls @computer'

# recursive grep
rgrep () {
      (
        if [[ $# -eq 0 || $1 == "--help" ]]; then
		echo "usage: rgrep [<DIR>] <REGEXP> <FILENAME_PATTERN>"
	fi
        if [ $# -gt 2 ]; then
          cd $1      
          shift
        fi
        find . -type f -name "$2" -print0 | xargs -0 grep -n "$1"
      )
}

# recursive grep in project
prgrep () {
        if [[ $# -eq 0 || $1 == "--help" ]]; then
		echo "usage: prgrep <REGEXP> <FILENAME_PATTERN>"
	fi
	pfind -type f -name "$2" -print0 | xargs -0 grep -n "$1"
}
