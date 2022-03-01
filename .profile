# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

PATH=$HOME/.local/bin:$PATH

SCOOP_SHIMS=$(cygpath -u $SCOOP)/shims
PATH=$SCOOP_SHIMS:$PATH


source <(direnv stdlib)

PATH_add_if_exists(){
    local new_path=$1
    if test -d $new_path ; then
	PATH_add $new_path
    fi
}


PATH_add_if_exists $HOME/bin

PATH_add_if_exists $HOME/.python3.7.7/bin

PATH_add_if_exists $HOME/.gem/ruby/2.5.0/bin

### RUST setup ###
PATH_add_if_exists $HOME/.cargo/bin
[[ -f $HOME/.cargo/env ]] && source $HOME/.cargo/env

PATH_add_if_exists $HOME/.npm-packages

# TODO: make this apply only on Windows
if [[ $OS == "Windows_NT" ]] ; then
    export MSYS2_ARG_CONV_EXCL=/C

    for d in $HOME/.rustup/toolchains/*/bin ; do
	PATH_add_if_exists $d
    done
fi

IOPATH="/c/IoLanguage"
IOBIN=$IOPATH/bin
IOLIB=$IOPATH/lib

PATH_add_if_exists $IOPATH/bin
PATH_add_if_exists $IOPATH/lib

export TERMINAL="alacritty"
export EDITOR=em
export VISUAL=emacs
export BROWSER=firefox

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
PATH_add_if_exists $HOME/.rvm/bin

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

export PROG_FILES_UNIX_PATH=$(cygpath -u "$PROGRAMFILES")
if [[ -f "$PROG_FILES_UNIX_PATH/SyncTrayzor/syncthing.exe" ]] ; then
    export ST_EXE_PATH="$PROG_FILES_UNIX_PATH/SyncTrayzor/syncthing.exe"
    export ST_SYNC_DIR=$(cygpath -u $("$ST_EXE_PATH" -paths | rg -i 'default sync folder directory' -A1 | tail -n 1 | tr -d ' \t\r\n'))
fi

if [[ ! -z "$ST_SYNC_DIR" ]] ; then
    # compgen -G ~/Sync/utility-software/arturo-\*Win\*
    ARTURO_HOME="${ST_SYNC_DIR}/utility-software/arturo-*Win*"
    [[ -d "$ARTURO_HOME" ]] && export ARTURO_HOME
    PATH_add_if_exists "$ARTURO_HOME"
fi


# if running bash
if [ -n "$BASH_VERSION" ]; then
    BASHRC=$HOME/.bashrc
    # If running interactively, and .bashrc if it exists, source it
    if [[ $- == *i* ]] && [[ -f $BASHRC ]] ; then
	. $BASHRC
    fi
fi
