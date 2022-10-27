# enable color support of ls
if [ -x /usr/bin/dircolors ]; then
    test -r $HOME/.dircolors && source <(dircolors -b $HOME/.dircolors) || source <(dircolors -b)
fi
