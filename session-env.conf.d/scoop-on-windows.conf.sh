if [[ $OS == "Windows_NT" ]] ; then
    SCOOP_SHIMS=$(cygpath -u $SCOOP)/shims
    PATH=$SCOOP_SHIMS:$PATH
fi
