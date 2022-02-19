
SOURCE=${BASH_SOURCE[0]}
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )
  SOURCE=$(readlink "$SOURCE")
  [[ $SOURCE != /* ]] && SOURCE=$DIR/$SOURCE # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done

export DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )

echo $DIR-$(date) | tee ./tst.txt

function terminal(){
    alacritty
}

function web-browser(){
    firefox.exe
}

function window-manager-restart(){
    powershell -File $DIR/../komorebi/komorebi.init.ps1
}

function window-manager-control(){
    komorebic $@
}

function window-manager-config(){
    $DIR/../komorebi/komorebi-configure.sh
}


function kill-kmonad(){
    powershell -File $DIR/service-kmonad.ps1 -Action kill-kmonad
}

function restart-kmonad(){
    powershell -File $DIR/service-kmonad.ps1 -Action restart-kmonad
}

function wm-help(){
    komorebic --help 2>&1 | ./slint-show.sh
}

echo HIIII
CMD=$1
shift
$CMD $@
