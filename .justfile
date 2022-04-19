set windows-powershell := true

WIN_HOME_DIR := env_var_or_default("USERPROFILE", "")
UNIX_HOME_DIR := env_var_or_default("HOME", "")
HOME_DIR := if path_exists(UNIX_HOME_DIR) == "true" { UNIX_HOME_DIR } else { WIN_HOME_DIR }

CONFIG_DIR := join(justfile_directory(),".config")
KMONAD_DIR := join(CONFIG_DIR,"kmonad")
KOMOREBI_DIR := join(CONFIG_DIR,"komorebi")
SYNCTHING_DIR := env_var_or_default("SYNCTHING_DIR", join(HOME_DIR,"Sync"))
CONF_SCRIPTS_DIR := join(CONFIG_DIR,"scripts")
SCOOP_DIR := env_var_or_default("SCOOP_DIR", join(HOME_DIR,"scoop"))
SCOOP_BUCKET_DIR := join(SCOOP_DIR,"buckets","my-bucket")


ENV_VAR_ACCESS := "$" + if os_family() == "windows" { "env:" } else { "" }
do-at JUST_PATH +ARGS:
	just -f {{ENV_VAR_ACCESS}}{{JUST_PATH}}/justfile {{ARGS}}



log-activity WHAT *COMMENTS:
	printf "%s,%s,{{WHAT}}\n" $(date '+%Y-%m-%d %H:%M:%S') {{COMMENTS}} >> {{SYNCTHING_DIR}}/activities_log/activities.csv


terminal:
	wezterm

web-browser:
	firefox

file-explorer:
	start ~

wm +ARGS:
	just -f {{KOMOREBI_DIR}}/justfile {{ARGS}}

kmonad +ARGS:
	just -f {{KMONAD_DIR}}/justfile {{ARGS}}
	just notify "kmonad {{ ARGS  }} done" ; true

wm-help:
	komorebic --help | just wm-show

wm-show:
	#!/usr/bin/env arturo
	a: []
	loop.forever ['a] [] [try? [a: a ++ input ""] else [break]]
	webview join.with:"<br>" a

quick-edit:
	alacritty.exe -o window.decorations=none -e micro

notify MSG *ARGS:
	notifu64 -m '{{MSG}}' {{ARGS}}
