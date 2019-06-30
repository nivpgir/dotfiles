#!/bin/bash

# assuming git repos are downloaded as submodules
function setup-light() {
    pushd light
    ./autogen.sh
    ./configure --prefix=$HOME/.local/ --exec-prefix=$HOME/.local
    make
    make install
    popd
}

function setup-screenful () {
    pushd screenful
    sudo cp 98-screen-detect.rules /etc/udev/rules.d
    sudo cp notify-awesome /lib/udev
    ln -sf screenful/screenful.lua ~/.config/awesome/
    ln -sf screenful/screens_db.lua ~/.config/awesome/
    popd
}

setup-light
setup-screenful
