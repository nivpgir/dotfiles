
(use-package eglot
  :straight nil
  :commands eglot
  )

(use-package lang-python
  :straight eglot
  :hook
  (python-mode . eglot-ensure))

(provide 'lang-python-eglot)
