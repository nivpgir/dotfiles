
for confile in $PIAMH_CONF_DIR/interactive-bash-session.conf.d/* ; do
    case $confile in
	*.disabled.sh)
	    :
	    ;;
	*)
	    source $confile
	    ;;
    esac
done
