#
# ~/.bashrc


alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'
if [ -f .bash_aliases ]; then
    . .bash_aliases
fi

### Completioning ###
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


# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000000
HISTFILESIZE=2000000
HISTFILE=$HOME/.histfile
export HISTCONTROL=ignoredups
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && source <(SHELL=/bin/sh lesspipe)


# enable color support of ls
if [ -x /usr/bin/dircolors ]; then
    # test -r $HOME/.dircolors && eval "$(dircolors -b $HOME/.dircolors)" || eval "$(dircolors -b)"
    test -r $HOME/.dircolors && source <(dircolors -b $HOME/.dircolors) || source <(dircolors -b)
fi

# colored GCC warnings and errors
# export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

### UTILITIES ###
function strip() {
    local charset=' \t\r\n'
    if [[ "$1" == "-" ]] ; then
	tr -d $charset
    else
	printf "$*" | tr -d "$charset"
    fi
}

function until-fail() {
    local cmd=$@
    echo executing: $cmd
    local i=0
    while eval $cmd; do
	echo end of iteration: $((i++))
    done
    echo failed on iteration: $i
}

function mount-tar(){
    which archivemount 2>&1 > /dev/null ||
	( echo "archivemount not found, returning..." && return )
    test ! -z $1 ||
	( echo "must give an archive to mount as an argument"  && return )

    local ARCHIVE=$1
    local MOUNT=/tmp/${USER}/${ARCHIVE%%.*}
    mkdir -p ${MOUNT}
    archivemount ${ARCHIVE} ${MOUNT}
}



### NPM ###

export MANPATH="$MANPATH:$NPM_PACKAGES/share/man"

### NVM ###
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

### Python virtualenv ###
export WORKON_HOME=$HOME/.py_venvs
[[ -f $HOME/.local/bin/virtualenvwrapper.sh ]] && source $HOME/.local/bin/virtualenvwrapper.sh


### nix setup ###
[[ -f $HOME/.nix-profile/etc/profile.d/nix.sh ]] && . $HOME/.nix-profile/etc/profile.d/nix.sh


# Wasmer
export WASMER_DIR="/home/nivp/.wasmer"
[ -s "$WASMER_DIR/wasmer.sh" ] && source "$WASMER_DIR/wasmer.sh"


# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.

export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).


source <(direnv hook bash)

if [[ $OS == "Windows_NT" ]] ; then
    _direnv_hook() {
	local previous_exit_status=$?;
	source <(MSYS_NO_PATHCONV=1 "direnv" export bash | sed 's|export PATH=|export _X_DIRENV_PATH=|g')
	if [ -n "$_X_DIRENV_PATH" ]; then
	    _X_DIRENV_PATH=$(cygpath -p "$_X_DIRENV_PATH")
	    export "PATH=$_X_DIRENV_PATH"
	    unset _X_DIRENV_PATH
	fi
	return $previous_exit_status;
    };

    if ! [[ "$PROMPT_COMMAND" =~ _direnv_hook ]]; then
	PROMPT_COMMAND="_direnv_hook;$PROMPT_COMMAND"
    fi
fi


# install/update with:`sh -c "$(curl -fsSL https://starship.rs/install.sh)" -- -b ~/.local/bin/ -V`
source <(starship init bash)
