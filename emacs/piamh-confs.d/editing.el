;;; editing
(use-package volatile-highlights
  :diminish
  :straight t
  :config
  (volatile-highlights-mode t)
  (when (featurep 'undo-tree)
    (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
    (vhl/install-extension 'undo-tree))
  )

(use-package visual-regexp
  :straight t
  )

(use-package visual-regexp-steroids
  :straight t
  :requires visual-regexp
  :config
  (my-leader-def
    "r" 'vr/replace
    "q" 'vr/query-replace)
  )

(column-number-mode)
(setq-default show-trailing-whitespace t)
(require 'tramp)
(global-unset-key (kbd "M-m"))
(global-auto-revert-mode t)

(use-package ediff
  :straight t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  :general
  (my-leader-def
    "D f" 'ediff-files
    "D b" 'ediff-buffers
    "D c" 'ediff-current-file)
  )

(setq visible-bell t)
(global-display-line-numbers-mode) ;; (global-linum-mode t)
(setq auto-save-list-file-prefix (expand-file-name "auto-save-list" user-emacs-directory))
(setq backup-directory-alist `(("." . "~/.saves")))
(delete-selection-mode t)
(show-paren-mode t)
(use-package ztree
  :straight t)


(use-package smartparens
  :straight t
  :defer t
  :custom
  (show-paren-style 'expression)
  :config
  (smartparens-global-mode))


(use-package expand-region
  :straight t
  :general
  ("M-h" 'er/expand-region))

(use-package undo-tree
  :diminish
  :straight t
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist `((".*" . ,(expand-file-name "auto-save/undo-tree-history" user-emacs-directory))))
  :general
  (:keymaps 'undo-tree-map
	    "C-/" nil
	    "C-?" nil
	    )

  (my-def
    "<undo>" 'undo-tree-undo
    "<redo>" 'undo-tree-redo
    "C-z" "<undo>"
    "C-S-z" "<redo>")

  ;;this has to stay in the global map otherwise undo-tree thinks it
  ;;shouldn't be activated, see undo tree documentation for more info
  ("C-/" 'comment-line)
  )

(use-package move-text
  :straight t
  :general
  ("C-M-i" 'move-text-up)
  ("C-M-k" 'move-text-down)
  )

(use-package wgrep
  :straight t)


(use-package string-inflection
  :init
  (defun piamh/string-inflection-cycle-auto ()
    "switching by major-mode"
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
     ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
     ((eq major-mode 'java-mode)
      (string-inflection-java-style-cycle))
     ((eq major-mode 'elixir-mode)
      (string-inflection-elixir-style-cycle))
     (t
      (string-inflection-ruby-style-cycle))))
  :general
  (my-leader-def
    "c" 'piamh/string-inflection-cycle-auto))

(use-package emacs
  :config
  (indent-tabs-mode -1)
  (editorconfig-mode t)
  )

