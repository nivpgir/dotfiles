#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
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
if test -d ~/.local/share/bash-completion/completions/ ; then
    for bcfile in ~/.local/share/bash-completion/completions/* ; do
	$bcfile
	. $bcfile
    done
fi
if test -d ~/.local/etc/bash_completion.d ; then
    if [ -d ~/.local/etc/bash_completion.d ]; then
	for f in ~/.local/etc/bash_completion.d/*; do
            . $f
	done
    fi
fi

export EDITOR=em
export VISUAL=emacs
export BROWSER='chromium'

RUBY_BUNDLE_BIN_DIR=$HOME/.gem/ruby/2.3.0/bin
[ -d $RUBY_BUNDLE_BIN_DIR ] && export PATH=$RUBY_BUNDLE_BIN_DIR:$PATH

function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

function venv_name {
    local venvname=${VIRTUAL_ENV##*/}
    if [[ -z "$venvname" ]]; then
	echo ""
    else
	echo "($venvname)"
    fi
}

# install/update with:`sh -c "$(curl -fsSL https://starship.rs/install.sh)" -- -b ~/.local/bin/ -V`
eval "$(starship init bash)"

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
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


# enable color support of ls
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# colored GCC warnings and errors
# export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'


### UTILITIES ###
function until_fail() {
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
NPM_PACKAGES="${HOME}/.npm-packages"
export PATH="$PATH:$NPM_PACKAGES/bin"
# Preserve MANPATH if I already defined it somewhere in my config.
export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"

### NVM ###
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

### Ruby Setup ###
RUBY_BIN_PATH=$HOME/.gem/ruby/2.3.0/bin/
[[ -d $RUBY_BIN_PATH ]] && PATH=$PATH:$RUBY_BIN_PATH

### Python virtualenv ###
export WORKON_HOME=~/.py_venvs
[[ -f ~/.local/bin/virtualenvwrapper.sh ]] && source ~/.local/bin/virtualenvwrapper.sh

### RUST setup ###
[[ -f $HOME/.cargo/env ]] && source $HOME/.cargo/env

### nix setup ###
[[ -f $HOME/.nix-profile/etc/profile.d/nix.sh ]] && . $HOME/.nix-profile/etc/profile.d/nix.sh

[[ -f $HOME/.local_bashrc ]] && . $HOME/.local_bashrc


alias vq="vaquero"
alias vqrepl="rlwrap vaquero repl"


if command -v pueue >/dev/null 2>&1 ; then
    alias pue="pueue"
    alias puest="pue status"
    alias puead="pue add --"
    alias puel="pue log"
    alias puesh="pue show"
    # echo trying to start pueue daemon...
    # pueued --daemonize
fi


# Wasmer
export WASMER_DIR="/home/nivp/.wasmer"
[ -s "$WASMER_DIR/wasmer.sh" ] && source "$WASMER_DIR/wasmer.sh"


# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).


IOPATH="/c/IoLanguage"
IOBIN=$IOPATH/bin
IOLIB=$IOPATH/lib
export PATH="$PATH:$IOBIN:$IOLIB"


eval "$(direnv hook bash)"

_direnv_hook() {
  local previous_exit_status=$?;
  eval "$(MSYS_NO_PATHCONV=1 "direnv.exe" export bash | sed 's|export PATH=|export _X_DIRENV_PATH=|g')";
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
