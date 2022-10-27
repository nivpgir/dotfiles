if [[ $OS == "Windows_NT" ]] ; then
    IOPATH="/c/IoLanguage"
    IOBIN=$IOPATH/bin
    IOLIB=$IOPATH/lib

    add_to_PATH_if_is_dir $IOPATH/bin
    add_to_PATH_if_is_dir $IOPATH/lib
fi
