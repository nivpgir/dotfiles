(use-package yaml-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("//.yml//'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("//.yaml//'" . yaml-mode)))
