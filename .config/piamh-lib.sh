add_to_PATH(){
    local new_path=$1
    local where=$2
    case "$PATH $position" in
	"*$new_path* *")
	    return
	    ;;
	*) ;;
    esac
    case $where in
	start)
	    PATH=$new_path:$PATH
	    ;;
	end)
	    PATH=$PATH:$new_path
	    ;;
	*)
	    PATH=$PATH:$new_path
	    ;;
    esac
}

add_to_PATH_if_is_dir(){
    local new_path=$1
    if test -d $new_path ; then
	add_to_PATH $new_path
    fi
}

function strip() {
    local charset=' \t\r\n'
    if [[ "$1" == "-" ]] ; then
	tr -d $charset
    else
	printf "$*" | tr -d "$charset"
    fi
}

function until-fail() {
    local cmd=$@
    echo executing: $cmd
    local i=0
    while eval $cmd; do
	echo end of iteration: $((i++))
    done
    echo failed on iteration: $i
}

function mount-tar(){
    which archivemount 2>&1 > /dev/null ||
	( echo "archivemount not found, returning..." && return )
    test ! -z $1 ||
	( echo "must give an archive to mount as an argument"  && return )

    local ARCHIVE=$1
    local MOUNT=/tmp/${USER}/${ARCHIVE%%.*}
    mkdir -p ${MOUNT}
    archivemount ${ARCHIVE} ${MOUNT}
}

current-file(){
    local SOURCE=${BASH_SOURCE[0]}
    local DIR=""
    while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
	DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )
	SOURCE=$(readlink "$SOURCE")
	[[ $SOURCE != /* ]] && SOURCE=$DIR/$SOURCE # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
    done
}

function timestamp {
    date '+%Y-%m-%d_%H:%M:%S'
}
