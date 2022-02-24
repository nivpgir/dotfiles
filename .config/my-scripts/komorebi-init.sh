#!/bin/bash

CONTAINER_PADDING=2
WORKSPACE_PADDING=0
komorebic.exe container-padding 1 0 $CONTAINER_PADDING
komorebic.exe container-padding 0 0 $CONTAINER_PADDING

komorebic.exe workspace-padding 1 0 $WORKSPACE_PADDING
komorebic.exe workspace-padding 0 0 $WORKSPACE_PADDING
komorebic.exe float-rule class SunAwtDialog

komorebic.exe retile
