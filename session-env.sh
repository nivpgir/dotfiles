

SOURCE=${BASH_SOURCE[0]}
DIR=""
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )
    SOURCE=$(readlink "$SOURCE")
    [[ $SOURCE != /* ]] && SOURCE=$DIR/$SOURCE # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done

export PIAMH_CONF_DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )



source $PIAMH_CONF_DIR/piamh-lib.sh
for confile in $PIAMH_CONF_DIR/session-env.conf.d/* ; do
    case $confile in
	*.disabled.sh)
	    :
	    ;;
	*)
	    source $confile
	    ;;
    esac
done
