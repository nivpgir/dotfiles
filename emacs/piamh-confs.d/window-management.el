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
  ("M-I" 'piamh/manage-windows)
  )


;; (use-package zoom
;;   :diminish
;;   :custom
;;   (zoom-ignored-major-modes '(dired-mode ediff-mode dirvish))
;;   (zoom-buffer-name-regexps '("^*Ediff.*" "^*ediff.*"))
;;   (zoom-mode t)
;;   (zoom-size '(0.618 . 0.618))
;;   :general
;;   ("C-x +" 'zoom)

(use-package popper
  :after magit
  :general
  ("C-<" 'popper-cycle)
  ("C-," 'popper-toggle-latest)
  ("C-." 'popper-toggle-type)
  ;; ("M" 'self-insert-command)
  ;; ("C-M" 'newline)
  ;; ("m" 'self-insert-command)
  ;; ("C-m" 'newline)
  (:keymaps 'quickrun--mode-map
	    "M" 'maximize-window
	    "m" 'minimize-window)

  
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "\\*eat\\*"
     "\\*eldoc\\*"
     "\\*warning\\*"
     "\\*EGLOT .* events\\*"
     "\\*quickrun\\*"
     help-mode
     compilation-mode
     flycheck-mode
     magit-process-mode))

  :config
  (popper-mode +1)
  (popper-echo-mode +1)
  (defun popper-popup-buffer-p (&optional buffer)
    (let ((buffer (or buffer (current-buffer))))
      (pcase (buffer-local-value 'popper-popup-status buffer)
	('raised nil)
	('popup t)
	('user-popup t)
	(_ (popper-popup-p buffer))))
    )

  (defun popper-close-popup-window ()
    (interactive)
    (if (popper-popup-buffer-p)
	(delete-window)))

  )

(use-package eldoc-box
  :straight '(eldoc-box
	      :type git
	      :host github
	      :repo "casouri/eldoc-box"
	      :files ("*.el" "*.org"))
  :hook
  ;; (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  (eglot-managed-mode . eldoc-box-hover-mode)
  :custom
  (eldoc-box-clear-with-C-g t)
  ;; :config
  ;; (eldoc-box-hover-at-point-mode)
  )

(require 'eldoc)
(setq eldoc-echo-area-prefer-doc-buffer nil)

'()

