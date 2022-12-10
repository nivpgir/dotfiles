(use-package dart-mode
  :straight t)

(use-package lsp-dart
  :straight t
  :after lsp-mode
  :config
  (setq read-process-output-max (* 1024 1024)))

(use-package hover
  :straight t
  :after lsp-dart)

