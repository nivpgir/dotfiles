(use-package helpful
  :straight t
  :general
  ("C-h f" 'helpful-callable)
  ("C-h v" 'helpful-variable)
  ("C-h k" 'helpful-key)
  ("C-h F" 'helpful-function)
  ("C-h ." 'helpful-at-point)
  )

(use-package which-key
  :straight t
  :config
  (which-key-mode t)
  ;; (which-key-setup-minibuffer)
  :custom
  (which-key-popup-type 'minibuffer)
  )
