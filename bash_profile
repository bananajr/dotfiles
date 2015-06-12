# put anything here that we only want to use for login shells (that is,
# when the username and password are entered).  things that should be
# executed for all interactive shells should go in bashrc.
#
# note: Terminal.app in OSX runs bash as a login shell for every instance.

# include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

LS_COLORS="di=34:ln=37:ex=31"
export LS_COLORS

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
