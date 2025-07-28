if ! command -v direnv 2>&1 >/dev/null ; then
    return
fi
source <(direnv hook bash)
source <(direnv stdlib)
