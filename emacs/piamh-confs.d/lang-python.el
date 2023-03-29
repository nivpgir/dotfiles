;; (load-file (expand-file-name "lang-python-eglot"))
(use-package poetry)
(require 'lang-python-eglot)
(use-package emacs
  :straight nil
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")
  )


