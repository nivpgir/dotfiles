if ! command -v zellij 2>&1 >/dev/null ; then
    return
fi
case $TERM in
    *eat-truecolor*)
	return
	;;
    dumb)
	return
	;;
esac

if [[ -z "$ZELLIJ" ]]; then
    if [[ "$ZELLIJ_AUTO_ATTACH" == "true" ]]; then
        zellij attach -c
    else
        zellij
    fi

    if [[ "$ZELLIJ_AUTO_EXIT" == "true" ]]; then
        exit
    fi
fi
