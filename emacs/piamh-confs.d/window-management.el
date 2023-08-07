;;; window management

(use-package winner
  :custom
  (winner-dont-bind-my-keys t)
  :config
  (winner-mode)
  )

(defun piamh/switch-or-manage-windows ()
  (interactive)
  (if (<= 2 (length (window-list)))
      (other-window 1)
    (piamh/manage-windows)))

(transient-define-prefix piamh/manage-windows ()
  "Manage window transient."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-quit-one
  ["Manage windows\n"
   [("j" "Go to the window on the left" windmove-left)
    ("k" "Go to the window below" windmove-down)
    ("i" "Go to the window above" windmove-up)
    ("l" "Go to the window on the right" windmove-right)
    ("-" "Split vertically" split-window-vertically)
    ("/" "Split horizontally" split-window-horizontally)
    ("o" "Jump to a window" switch-window)
    ("c" "Close window" delete-window)
    ("u" "Undo last window change" winner-undo)
    ("r" "resize window mode" piamh/resize-windows)
    ("f" "make full window (delete other windows)" delete-other-windows)
    ]])

(transient-define-prefix piamh/resize-windows ()
  "Resize windows transient."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-exit
  ["Manage windows\n"
   [("h" "Enlarge sideways" (lambda () (interactive) (enlarge-window-horizontally 1)))
    ("H" "Shrink sideways" (lambda () (interactive) (enlarge-window-horizontally -1)))
    ("u" "Enlarge vertically" (lambda () (interactive) (enlarge-window 1)))
    ("U" "Shrink vertically" (lambda () (interactive) (enlarge-window -1)))
    ]])

(use-package switch-window
  :custom
  (switch-window-shortcut-style 'qwerty)
  (switch-window-minibuffer-shortcut ?z)
  (switch-window-auto-resize-window nil)
  :general
  ("M-i" 'piamh/switch-or-manage-windows)
  ("M-o" 'piamh/manage-windows)
  )


;; (use-package zoom
;;   :diminish
;;   :custom
;;   (zoom-ignored-major-modes '(dired-mode ediff-mode dirvish))
;;   (zoom-buffer-name-regexps '("^*Ediff.*" "^*ediff.*"))
;;   (zoom-mode t)
;;   (zoom-size '(0.618 . 0.618))
;;   :general
;;   ("C-x +" 'zoom))

(use-package popper
  :ensure t ; or :straight t
  :general
  ("C-<" 'popper-toggle-latest)
  ("C-," 'popper-cycle)
  ("C-." 'popper-toggle-type)
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     help-mode
     compilation-mode
     flycheck-mode
     magit-process))

  :config
  (popper-mode +1)
  (popper-echo-mode +1))
