set windows-powershell := true

WIN_HOME_DIR := env_var_or_default("USERPROFILE", "")
UNIX_HOME_DIR := env_var_or_default("HOME", "")
HOME_DIR := if "true" == path_exists(UNIX_HOME_DIR) { UNIX_HOME_DIR } else { WIN_HOME_DIR }

CONFIG_DIR := join(HOME_DIR,".config")

export SCOOP_BUCKET_DIR := join(HOME_DIR,"buckets/my-bucket")
export KMONAD_DIR := join(CONFIG_DIR,"kmonad")
export KOMOREBI_DIR := join(CONFIG_DIR,"komorebi")
export CONF_SCRIPTS_DIR := join(CONFIG_DIR,"my-scripts")


ENV_VAR_ACCESS := "$" + if os_family() == "windows" { "env:" } else { "" }
do-at JUST_PATH +ARGS:
	just -f {{ENV_VAR_ACCESS}}{{JUST_PATH}}/justfile {{ARGS}}


