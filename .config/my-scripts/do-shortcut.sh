
SOURCE=${BASH_SOURCE[0]}
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  SCRIPT_DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )
  SOURCE=$(readlink "$SOURCE")
  [[ $SOURCE != /* ]] && SOURCE=$SCRIPT_DIR/$SOURCE # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done

export SCRIPT_DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )

WM_CONF_DIR=$SCRIPT_DIR/../komorebi
PATH=$SCRIPT_DIR:$WM_CONF_DIR:$PATH

function terminal(){
    alacritty
}

function web-browser(){
    firefox
}

function window-manager-restart(){
    powershell -File $WM_CONF_DIR/komorebi.init.ps1
}

function window-manager-control(){
    komorebic $@
}

function window-manager-config(){
    komorebi-configure.sh
}

function kill-kmonad(){
    powershell -File $SCRIPT_DIR/kmonad-service.ps1 -Action kill-kmonad
}

function restart-kmonad(){
    powershell -File $SCRIPT_DIR/kmonad-service.ps1 -Action restart-kmonad
}

function wm-help(){
    komorebic --help 2>&1 | runner $SCRIPT_DIR/egui-show.rs
}

CMD=$1
shift
$CMD $@ >> kmonad-shortcut.log.txt
