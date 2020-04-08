#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use 
# polybar-msg cmd quit

# Launch bar1 and bar2
echo "---" | tee -a /tmp/polybar-mon1.log
echo "---" | tee -a /tmp/polybar-mon2.log
# polybar example >> /tmp/polybar-example.log 2>&1 &
for mon in mon1 mon2 mon3 ; do
    $HOME/.local/bin/polybar $mon >> /tmp/polybar-$mon.log 2>&1 &
done
# $HOME/.local/bin/polybar mon2 >> /tmp/polybar-mon2.log 2>&1 &
# $HOME/.local/bin/polybar mon3 >> /tmp/polybar-mon3.log 2>&1 &
echo "Bars launched..."
