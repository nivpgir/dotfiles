#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
# If all your bars have ipc enabled, you can also use 
# polybar-msg cmd quit

# Launch bar1 and bar2
echo "---" | tee -a /tmp/polybar-example.log
# polybar example >> /tmp/polybar-example.log 2>&1 &
$HOME/.local/bin/polybar example >> /tmp/polybar-example.log 2>&1 &
echo "Bars launched..."
