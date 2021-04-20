#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use 
# polybar-msg cmd quit

# Launch bar1 and bar2
echo "---" | tee -a /tmp/polybar.log

[[ -f $HOME/.local/bin/polybar  ]] && POLYBAR_BIN="$HOME/.local/bin/polybar"
[[ `which polybar`  ]] && POLYBAR_BIN="polybar"
POLYBAR_CMD="$POLYBAR_BIN base"
POLYBAR_LOG="/tmp/polybar.log"
POLYBAR_ENV="$HOME/.config/polybar/polybar-env"
LAUNCH_CMD=$POLYBAR_CMD
if test -f $POLYBAR_ENV ; then
    source $POLYBAR_ENV
fi
echo $LAUNCH_CMD >> $POLYBAR_LOG &
$LAUNCH_CMD >> $POLYBAR_LOG &

echo "Bars launched..."
