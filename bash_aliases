LS_COLORS="di=34:ln=37:ex=31"
export LS_COLORS

if [ $OS == osx ]; then
	alias ls='ls -G'
elif [ $OS == linux ]; then
	alias ls='ls --color'
fi

alias thumbdis='objdump -S -M reg-names-std --disassembler-options=force-thumb -d \!:*';
