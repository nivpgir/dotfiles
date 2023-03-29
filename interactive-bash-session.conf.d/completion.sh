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

# load local bash completions:
# completions may be installed in various locations when installing to the global env
# since we are trying to emulate that same env under ~/.local we need to source
# the same locations.
if test -d $HOME/.local/share/bash-completion/completions/ ; then
    for bcfile in $HOME/.local/share/bash-completion/completions/* ; do
	$bcfile
	. $bcfile
    done
fi
if test -d $HOME/.local/etc/bash_completion.d ; then
    if [ -d $HOME/.local/etc/bash_completion.d ]; then
	for f in $HOME/.local/etc/bash_completion.d/*; do
            . $f
	done
    fi
fi
