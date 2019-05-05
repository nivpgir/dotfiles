#!/bin/bash

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# some more ls aliases
alias ls='ls -CFh --color=auto'
alias ll='ls -lhF --color=auto'
alias lal='ls -alhF --color=auto'
alias lla='ls -alhF --color=auto'
alias la='ls -ahF --color=auto'
alias l='ls -CFh --color=auto'

# prettier pushd stack printing
alias d='dirs -v'

alias mv='mv -i'

EMACS_CLIENT="emacsclient -q -c -n -a ''"
EMACS_TERM_CLIENT="emacsclient -q -c -t -a ''"
# alias em="$EMACS_TERM_CLIENT -t $@"
alias em="emacs --no-window-system $@"
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
