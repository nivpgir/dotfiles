@echo off
echo %date%%time% %* >> C:/Users/Niv/.my-log.txt
start /w /b /d "C:/Users/Niv/.config/kmonad" %SCOOP%/apps/msys2/current/usr/bin/bash.exe --noprofile %* >> C:/Users/Niv/.my-log.txt
