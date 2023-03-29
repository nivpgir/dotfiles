# Usage: layout pipenv
#
# Similar to layout_python, but uses Pipenv to build a
# virtualenv from the Pipfile located in the same directory.
#
layout_pipenv() {
  PIPENV_PIPFILE="${PIPENV_PIPFILE:-Pipfile}"
  if [[ ! -f "$PIPENV_PIPFILE" ]]; then
    log_error "No Pipfile found.  Use \`pipenv\` to create a \`$PIPENV_PIPFILE\` first."
    exit 2
  fi

  # VIRTUAL_ENV=$(pipenv --venv ; true)
  VIRTUAL_ENV=$(cygpath -m $(pipenv --venv | tr -d '\r' 2>/dev/null ; true))
  VIRTUAL_ENV_UNIX_FORM=$(cygpath -u $VIRTUAL_ENV 2>/dev/null)
  echo VIRTUAL_ENV_UNIX_FORM is $VIRTUAL_ENV_UNIX_FORM

  if [[ -z $VIRTUAL_ENV_UNIX_FORM || ! -d $VIRTUAL_ENV_UNIX_FORM ]]; then
    pipenv install --dev
    # VIRTUAL_ENV=$(pipenv --venv ; true)
    VIRTUAL_ENV=$(cygpath -m $(pipenv --venv | tr -d '\r' 2>/dev/null ; true))
  fi


  export PIPENV_ACTIVE=1
  PATH_add "$VIRTUAL_ENV_UNIX_FORM/bin"
  PATH_add "$VIRTUAL_ENV_UNIX_FORM/Scripts"
  export VIRTUAL_ENV
}

