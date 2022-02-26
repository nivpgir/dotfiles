@echo off
echo %date%%time% %* >> C:/Users/Niv/.my-log.txt
start /w /b /d "C:/Users/Niv/.config/kmonad" "" %WINBASH% --noprofile %* >> C:/Users/Niv/.my-log.txt""
