if [ $OS == osx ]; then
	alias ls='ls -G'
elif [ $OS == linux ]; then
	alias ls='ls --color'
fi

alias thumbdis='objdump -S -M reg-names-std --disassembler-options=force-thumb -d \!:*';

alias gcl='git clone'
alias gco='git checkout'
alias gst='git status'
alias gbr='git branch'
