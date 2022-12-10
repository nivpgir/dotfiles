(use-package with-venv
  :straight t)

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"))

(use-package auto-virtualenv
  :straight t
  :config
  (require 'auto-virtualenv)
  :hook
  (python-mode . auto-virtualenv-set-virtualenv)
  (focus-in . auto-virtualenv-set-virtualenv)
  (window-configuration-change . auto-virtualenv-set-virtualenv))

(setq lsp-pyls-plugins-flake8-max-line-length 100)

