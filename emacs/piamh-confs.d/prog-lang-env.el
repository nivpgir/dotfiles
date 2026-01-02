
(use-package tree-sitter
  :straight t
  :hook
  ((c-mode-hook
    c++-mode-hook
    css-mode-hook
    html-mode-hook
    js-mode-hook
    js2-mode-hook
    python-mode-hook
    ruby-mode-hook
    rust-mode-hook
    typescript-mode-hook
    python-mode
    rustic-mode) . tree-sitter-hl-mode)
  )

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(straight-use-package 'tree-sitter-langs)

(use-package flycheck
  :straight t
  ;; :hook (prog-mode . flycheck-mode)
  )


(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  :hook
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  )

