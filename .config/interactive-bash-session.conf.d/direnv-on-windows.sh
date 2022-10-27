if [[ $OS == "Windows_NT" ]] ; then
    _direnv_hook() {
	local previous_exit_status=$?;
	source <(MSYS_NO_PATHCONV=1 "direnv" export bash | sed 's|export PATH=|export _X_DIRENV_PATH=|g')
	if [ -n "$_X_DIRENV_PATH" ]; then
	    _X_DIRENV_PATH=$(cygpath -p "$_X_DIRENV_PATH")
	    export "PATH=$_X_DIRENV_PATH"
	    unset _X_DIRENV_PATH
	fi
	return $previous_exit_status;
    };

    if ! [[ "$PROMPT_COMMAND" =~ _direnv_hook ]]; then
	PROMPT_COMMAND="_direnv_hook;$PROMPT_COMMAND"
    fi
fi
