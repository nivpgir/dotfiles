;;; window management

(use-package switch-window
  :custom
  (switch-window-shortcut-style 'qwerty)
  (switch-window-minibuffer-shortcut ?z)
  :general
  ("C-x o" 'switch-window)
  ("M-o" 'switch-window)
  )

(use-package zoom
  :custom
  (zoom-mode t)
  (zoom-size '(0.618 . 0.618))
  :general
  ("C-x +" 'zoom))

(use-package windmove
  :straight t
  :general
  ("C-S-j" 'windmove-left)
  ("C-S-l" 'windmove-right)
  ("C-S-i" 'windmove-up)
  ("C-S-k" 'windmove-down)
  )

;; (use-package winum
;;   :straight t
;;   :config
;;   (winum-mode)
;;   (general-def winum-keymap
;;     "M-0" 'winum-select-window-0-or-10
;;     "M-1" 'winum-select-window-1
;;     "M-2" 'winum-select-window-2
;;     "M-3" 'winum-select-window-3
;;     "M-4" 'winum-select-window-4
;;     "M-5" 'winum-select-window-5
;;     "M-6" 'winum-select-window-6
;;     "M-7" 'winum-select-window-7
;;     "M-8" 'winum-select-window-8
;;     "M-9" 'winum-select-window-9)
;;   )
