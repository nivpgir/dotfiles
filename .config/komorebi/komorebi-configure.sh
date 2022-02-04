#!/bin/bash

komorebic manage-rule exe alacritty.exe
komorebic float-rule exe Wox.exe
komorebic float-rule class SunAwtDialog
komorebic float-rule class TWizardForm

WORKSPACE_PADDING=0
CONTAINER_PADDING=3

komorebic container-padding 0 0 $CONTAINER_PADDING
komorebic workspace-padding 0 0 $WORKSPACE_PADDING

komorebic change-layout ultrawide-vertical-stack


komorebic identify-tray-application exe Discord.exe
komorebic identify-border-overflow exe Discord.exe

komorebic identify-tray-application exe Todoist.exe
komorebic identify-border-overflow exe Todoist.exe


komorebic.exe focus-follows-mouse enable
komorebic retile
