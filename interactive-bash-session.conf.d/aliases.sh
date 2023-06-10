#!/bin/bash
if [ -f $PIAMH_CONF_DIR/.bash_aliases ]; then
    . $PIAMH_CONF_DIR/.bash_aliases
fi

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# some more ls aliases
if which exa > /dev/null 2>&1; then
    LS=exa
    alias lt=$LS' -FTh --color=auto'
    alias llt=$LS' -FThl --color=auto'
else
    LS=ls
fi
alias ls=$LS' -Fh --color=auto'
alias l=$LS' -Fh --color=auto'
alias ll=$LS' -lhF --color=auto'
alias lal=$LS' -alhF --color=auto'
alias lla=$LS' -alhF --color=auto'
alias la=$LS' -ahF --color=auto'

if which fd > /dev/null 2>&1; then
    alias find=fd
fi

if which hexyl > /dev/null 2>&1; then
    alias hd=hexyl
    alias hexdump=hexyl
fi

# prettier pushd stack printing
alias d='dirs -v'

alias mv='mv -i'

EMACS_CLIENT="emacsclient -q -c -n -a ''"
EMACS_TERM_CLIENT="emacsclient -q -c -t -a ''"
# alias em="$EMACS_TERM_CLIENT -t $@"
alias em="emacs --no-window-system $@"
alias emq="emacs --no-window-system -Q $@"
alias emacs="emacs $@"

# Add an "alert" alias for long running commands.  Use like so:
# e.g  sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# git aliases
alias gc="git commit"
alias gs="git status"
alias gd="git diff"
alias gunadd="git reset HEAD "
alias gco="git checkout"
alias gcob="git checkout -b"
alias gclone="git clone --recursive"
alias gpush="git push"
alias gpull="git pull"
alias gfetch="git fetch"
alias ga="git add"
alias glp='git log --pretty=format:"%C(yellow)%h %Cblue%>(12)%ad %Cgreen%<(7)%aN%Cred%d %Creset%s"'

# calcualations:
function calc(){
    echo "$(( $@ ))"
}

function rename_workspace(){
    if [[ -z $1 ]]; then
        newname=`zenity --entry \
                        --title="new workspace name" \
                        --text="Workspace name:" \
                        --entry-text="IDK"`
    else
        newname=`echo $1`
    fi
    newname=`i3-msg -t get_workspaces |
                    jq  'map(select(.focused)) | .[] | .name' |
                    sed -r -e "s/\"([0-9]).*\"/\1:\1:$newname/"`
    i3-msg rename workspace to $newname
}

function tohex() {
    for n in $@; do
        printf "%X" $n
    done
    echo
}

function todec() {
    for n in $@; do
        printf "%d " $n
    done
    echo
}



# Lists folders and files sizes in the current folder
alias ducks='du -cksh * | sort -rh | head -11'

alias luajit="rlwrap luajit"

if command -v pueue >/dev/null 2>&1 ; then
    alias pue="pueue"
    alias puest="pue status"
    alias puead="pue add --"
    alias puel="pue log"
    alias puesh="pue show"
    # echo trying to start pueue daemon...
    # pueued --daemonize
fi



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

if command -v tydra >/dev/null 2>&1 ; then
    alias menu="tydra $PIAMH_CONF_DIR/tydra/tydra.yaml"
fi
