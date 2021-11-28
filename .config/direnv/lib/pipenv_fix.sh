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

  # VIRTUAL_ENV=$(pipenv --venv 2>/dev/null  | tr -d '\r\n' ; true)
  VIRTUAL_ENV=$(cygpath $(pipenv --venv 2>/dev/null ; true) 2>/dev/null)
  echo VIRTUAL_ENV is $VIRTUAL_ENV

  if [[  $PIPENV_ACTIVE -ne 1 ]] && [[ -z $VIRTUAL_ENV || ! -d $VIRTUAL_ENV ]]; then
    pipenv install --dev
    # VIRTUAL_ENV=$(pipenv --venv ; true)
    VIRTUAL_ENV=$(cygpath $(pipenv --venv 2>/dev/null ; true))
  fi

  export PIPENV_ACTIVE=1
  PATH_add "$VIRTUAL_ENV/bin"
  export VIRTUAL_ENV
}

