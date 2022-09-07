;;; init.el --- Initialization file for Emacs
;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary: Emacs Startup File --- initialization for Emacs

(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
  (when (file-exists-p bootstrap-file)
    (setq package-enable-at-startup nil)
    (setq gc-cons-threshold 100000000)

    (defvar file-name-handler-alist-original file-name-handler-alist)
    (setq file-name-handler-alist nil)

    (defvar better-gc-cons-threshold 67108864) ; 64mb
    (add-hook 'emacs-startup-hook
              (lambda ()
		(setq gc-cons-threshold better-gc-cons-threshold)
		(setq file-name-handler-alist file-name-handler-alist-original)
		(makunbound 'file-name-handler-alist-original)))

    (add-hook 'emacs-startup-hook
              (lambda ()
		(if (boundp 'after-focus-change-function)
                    (add-function :after after-focus-change-function
				  (lambda ()
                                    (unless (frame-focus-state)
                                      (garbage-collect))))
		  (add-hook 'after-focus-change-function 'garbage-collect))
		(defun gc-minibuffer-setup-hook ()
		  (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

		(defun gc-minibuffer-exit-hook ()
		  (garbage-collect)
		  (setq gc-cons-threshold better-gc-cons-threshold))

		(add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
		(add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
    ))


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)



(use-package niv-mode
  :straight '(niv-mode :local-repo "niv-mode")
  )



(use-package emacs
  :config
  (tool-bar-mode -1)
  :hook
  (prog-mode . electric-pair-local-mode)
  :custom
  auto-mode-alist (delete '("\\.oak\\'" . scheme-mode) auto-mode-alist))



;; not working/slow
;; (use-package sublimity
;;   :straight t
;;   :config
;;   (require 'sublimity)
;;   (require 'sublimity-scroll)
;;   (require 'sublimity-map)
;;   (sublimity-mode 0)
;;   )

(use-package general
  :straight t
  :config
  (general-def "C-z" nil)

  (general-create-definer my-def :keymaps 'niv-mode-map)
  (general-create-definer my-leader-def :prefix "C-c" :wrapping my-def)

  (my-leader-def
    "b" (general-key-dispatch (lambda () (interactive) (switch-to-buffer "*scratch*"))
	  :timeout 0.25
	  "b" 'niv/new-empty-buffer)
    "-" 'split-window-below
    "/" 'split-window-right
    "<backspace>" 'delete-window
    "<tab>" 'niv/alternate-buffer
    "RET" 'newline-and-indent
    "k w" 'delete-window
    "k b" 'kill-buffer
    "k l" 'kill-whole-line
    )

  (general-def "C-a" 'niv/prelude-move-beginning-of-line)
  (general-def "M-k" 'kill-whole-line)
  (general-def "C-x C-e" 'pp-eval-last-sexp)
  (general-def "M-n" (lambda () (interactive) (scroll-up 1)))
  (general-def "M-p" (lambda () (interactive) (scroll-down 1)))
  )



(use-package crux
  :straight t
  :general
  (my-leader-def
    "n" 'crux-cleanup-buffer-or-region
    "f" 'crux-recentf-find-file
    "e" 'crux-eval-and-replace
    "d" 'crux-duplicate-current-line-or-region
    "M-d" 'crux-duplicate-and-comment-current-line-or-region
    "R" 'crux-rename-file-and-buffer
    "I" 'crux-find-user-init-file
    "," 'crux-find-user-custom-file
    "C-u" 'crux-upcase-region
    "C-l" 'crux-downcase-region
    "M-c" 'crux-capitalize-region
    )
  ("S-RET" 'crux-smart-open-line)
  ("M-o" 'crux-other-window-or-switch-buffer)
  ("C-k" 'crux-kill-and-join-forward)
  )

(use-package eros
  :straight t
  :config
  (eros-mode 1)
  :general
  ("C-c C-e" 'eros-eval-last-sexp))

(use-package elisp-mode
  :straight emacs
  :general
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
	    "C-M-i" nil))

(use-package helpful
  :straight t
  :general
  ("C-h f" 'helpful-callable)
  ("C-h v" 'helpful-variable)
  ("C-h k" 'helpful-key)
  ("C-h F" 'helpful-function)
  (my-leader-def
    "C-d" 'helpful-at-point)
  )


(use-package volatile-highlights
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

;; (use-package hydra
;;   :straight t
;;   :init
;;   (defhydra hydra-wasd
;;     (my-keymap
;;      "C-n"
;;      :pre (set-cursor-color "#40e0d0")
;;      :post (progn (set-cursor-color "#ffffff")
;; 		  (message "Thank you, come again.")))
;;     "vi"
;;     ("d" forward-char)
;;     ("a" backward-char)
;;     ("s" next-line)
;;     ("w" previous-line)
;;     ("q" nil "quit")))
(defun rk/open-compilation-buffer (&optional buffer-or-name shackle-alist shackle-plist)
  "Helper for selecting window for opening *compilation* buffers."
  ;; find existing compilation window left of the current window or left-most window
  (let ((win (or (loop for win = (if win (window-left win) (get-buffer-window))
                       when (or (not (window-left win))
                                (string-prefix-p "*compilation" (buffer-name (window-buffer win))))
                       return win)
                 (get-buffer-window))))
    ;; if the window is dedicated to a non-compilation buffer, use the current one instead
    (when (window-dedicated-p win)
      (let ((buf-name (buffer-name (window-buffer win))))
        (unless (string-prefix-p "*compilation" buf-name)
          (setq win (get-buffer-window)))))
    (set-window-buffer win (get-buffer buffer-or-name))
    (set-frame-selected-window (window-frame win) win)))


(use-package shackle
  :straight t
  :diminish
  :custom
  (shackle-rules '((compilation-mode :custom rk/open-compilation-buffer :select t)
		   ("\\*rustic-compilation\\*" :custom rk/open-compilation-buffer :select t :inhibit-window-quit t)
		   ("\\*Apropos\\|Help\\|Occur\\|tide-references\\*" :regexp t :same t :select t :inhibit-window-quit t)
		   ("\\*magit" :regexp t :same t :select t)
		   ("\\*shell.*" :regexp t :same t :select t)
		   ("\\*PowerShell.*" :regexp t :same t :select t)
		   ("\\*Cargo.*" :regexp t :other t :select nil)
		   ("*Messages*" :select nil :other t)
		   ("*go-guru-output*" :select t :same t)
		   ("*Proced*" :select t :same t)
		   ("*Buffer List*" :select t :same t)
		   ("\\*Pp Eval Output\\*" :regexp t :same nil :select t :other t)
		   ("*Messages*" :same nil :other t :select t :inhibit-window-quit t)))

  (shackle-default-rule nil)
  :config
  (shackle-mode))

(use-package elgrep
  :straight t)

(use-package doct
  :straight t
  :commands (doct))


(use-package org
  :straight org-contrib
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (dot . t)))
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)	; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  ;; diff hooks for org mode
  ;; Check for org mode and existence of buffer
  (defun f-ediff-org-showhide(buf command &rest cmdargs)
    "If buffer exists and is orgmode then execute command"
    (if buf
	(if (eq (buffer-local-value 'major-mode (get-buffer buf)) 'org-mode)
            (save-excursion (set-buffer buf) (apply command cmdargs)))))

  (defun f-ediff-org-unfold-tree-element ()
    "Unfold tree at diff location"
    (f-ediff-org-showhide ediff-buffer-A 'org-reveal)
    (f-ediff-org-showhide ediff-buffer-B 'org-reveal)
    (f-ediff-org-showhide ediff-buffer-C 'org-reveal))

  (defun f-ediff-org-fold-tree ()
    "Fold tree back to top level"
    (f-ediff-org-showhide ediff-buffer-A 'hide-sublevels 1)
    (f-ediff-org-showhide ediff-buffer-B 'hide-sublevels 1)
    (f-ediff-org-showhide ediff-buffer-C 'hide-sublevels 1))

  (defun org-time-stamp-no-prompt ()
    (insert (format-time-string)
	    (org-time-stamp-format 'long)
	    (org-current-effective-time)))

  (defun org-time-stamp-inactive-no-prompt ()
    (insert (format-time-string)
	    (org-time-stamp-format 'long 'inactive)
	    (org-current-effective-time)))

  (defun org-habit-count-completed ()
    (count-matches
     (char-to-string org-habit-completed-glyph)
     (line-beginning-position) (line-end-position)))

  (defun org-habit-line-p (point)
    (get-text-property point 'org-habit-p))

  (defun insert-on-line-end (string)
    (save-excursion
      (end-of-line)
      (insert string)))

  (defmacro for-each-line-of-buffer (body)
    `(save-excursion
       (point-min)
       (while (not (eobp))
	 ,body
	 (forward-line 1))))

  (defun org-habit-streak-count ()
    (for-each-line-of-buffer
     (when (org-habit-line-p (point))
       (insert-on-line-end
	(number-to-string (org-habit-count-completed))))))
  :hook
  (org-after-todo-statistics . org-summary-todo)
  (ediff-select . f-ediff-org-unfold-tree-element)
  (ediff-unselect . f-ediff-org-fold-tree)
  :custom
  ;; for more complex stuff look at `org-depend.el'
  (org-enforce-todo-dependencies t "block setting to DONE until previous siblings and children are DONE")
  (org-enforce-todo-checkbox-dependencies t "same as above but for checkboxes")
  (org-cycle-separator-lines 0)
  (org-extend-today-until 3)
  (org-use-effective-time t)
  (org-log-reschedule 'time)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-targets '((nil :maxlevel . 3)
			(org-agenda-files :maxlevel . 9)))

  (org-capture-templates
   (doct '(("Todo"
	    :keys "t"
	    :file "~/Sync/organizing/pending-tasks.org"
	    :todo-state "TODO"
	    :template ("* %{todo-state} %^{Description}"
		       ":PROPERTIES:"
		       ":CREATED: %U"
		       ":END:"
		       "%a"
		       "%i%?"))
	   ("Event" :keys "e"
	    :template ("* %^{Who?/What?}"
		       ":PROPERTIES:"
		       ":CREATED: %U"
		       ":END:"
		       "%^{When}T"
		       "%a"
		       "%i%?")
	    :children (("Personal"
			:keys "p"
			:file "~/Sync/organizing/personal.org"
			:olp ("Events"))
		       ("Work"
			:keys "w"
			:file "~/Sync/organizing/cady/tasks.org"
			:olp ("Events")))))))
  :general
  (my-leader-def
    "c" 'org-capture
    "os" (general-key-dispatch 'org-sort
	   :timeout 0.25
	   "l" 'org-store-link)
    ;; "osl" 'org-store-link
    "odl" 'org-insert-last-stored-link
    )
  ("M-m s" 'org-todo)
  ("M-m b" 'org-metaleft)
  ("M-m f" 'org-metaright)
  ("C-M-<return>" 'org-insert-subheading)
  ("M-m t" 'org-insert-structure-template)
  ("C-M-S-<return>" 'org-insert-todo-subheading)
  )

(use-package org-agenda
  :straight org
  :after org
  :hook
  (org-agenda-finalize . org-habit-streak-count)
  :general
  (my-leader-def
    "a" 'org-agenda)
  :custom
  (org-agenda-files (list "~/Sync/organizing"))
  (org-agenda-start-on-weekday 6)
  ;; (org-agenda-start-day "-1d")
  (org-agenda-custom-commands nil)
  (org-agenda-sorting-strategy '((agenda habit-down time-up category-keep tag-up priority-down))
			       (todo priority-down category-keep)
			       (tags priority-down category-keep)
			       (search category-keep))
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                              ;; Indent todo items by level to show nesting
                              (todo . " %i %-12:c%l")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  (org-agenda-log-mode-items '(closed clock state))
  :config
  (add-to-list 'org-agenda-custom-commands
	       '("d" . "Todays views"))
  (add-to-list 'org-agenda-custom-commands
	       '("dd" agenda "Todays agenda" ((org-agenda-span 'day))))
  (add-to-list 'org-agenda-custom-commands
	       '("dc" agenda "Cadys agenda"
		 ((org-agenda-files '("~/Sync/organizing/cady/tasks.org"))
		  (org-agenda-start-day ".")
		  (org-agenda-span 'day))))
  (add-to-list 'org-agenda-custom-commands
	       '("dm" agenda "My agenda"
		 ((org-agenda-files '("~/Sync/organizing/personal.org"))
		  (org-agenda-start-day ".")
		  (org-agenda-span 'day))))
  (add-to-list 'org-agenda-custom-commands
               '("w" . "Weekly views"))
  (add-to-list 'org-agenda-custom-commands
               '("ww" "Weekly summary"
		 agenda ""
		 ((org-agenda-start-day "-1w")
                  (org-agenda-span 7)
                  (org-agenda-start-with-log-mode '(closed))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE ")))))
  (add-to-list 'org-agenda-custom-commands
               '("wc" "Cady weekly view"
		 agenda ""
		 ((org-agenda-files '("~/Sync/organizing/cady/tasks.org"))
		  (org-agenda-start-day "-1w")
                  (org-agenda-span 7)
                  (org-agenda-start-with-log-mode '(closed))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE ")))))
  (add-to-list 'org-agenda-custom-commands
               '("wm" "My weekly view"
		 agenda ""
		 ((org-agenda-files '("~/Sync/organizing/personal.org"))
		  (org-agenda-start-day "-1w")
                  (org-agenda-span 7)
		  (org-agenda-start-with-log-mode '(closed))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE ")))))
  )

(use-package org-habit
  :straight org-contrib
  :config
  (require 'org-habit)
  :custom
  (org-habit-preceding-days 60))

(use-package org-expiry
  :straight org-contrib
  :after org
  :custom
  (org-expiry-created-property-name "CREATED") ; Name of property when an item is created
  (org-expiry-inactive-timestamps   t)
  :config
  (advice-add 'org-insert-heading :after 'org-expiry-insert-created)
  )


(setq visible-bell t)

;; ;;use ibuffer instead of list-buffers
;; (global-set-key (kbd "C-x C-b") 'ibuffer)
;;enable line numbers always
(global-linum-mode t)
;;activate volatile highlights
;; (volatile-highlights-mode t)
;;make backup files only in "~/.saves":
(setq backup-directory-alist `(("." . "~/.saves")))
;;settings for gdb:
;; use gdb-many-windows by default
(setq gdb-many-windows t)
;; Non-nil means display source file containing the main routine at startup
(setq gdb-show-main t)
;; first things first...
;; delete selection mode
(delete-selection-mode t)
;; centered point consider using: https://www.emacswiki.org/emacs/centered-cursor-mode.el

;; match parens
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

(use-package which-key
  :straight t
  :config
  (which-key-mode t)
  (which-key-setup-minibuffer)
  :custom
  (which-key-popup-type 'minibuffer)
  )


;; expand region with M-h
(use-package expand-region
  :straight t
  :general
  ("M-h" 'er/expand-region))

;; undo-tree with diff on visualizing
(use-package undo-tree
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

(use-package windmove
  :straight t
  :general
  ("C-S-j" 'windmove-left)
  ("C-S-l" 'windmove-right)
  ("C-S-i" 'windmove-up)
  ("C-S-k" 'windmove-down)
  )

(use-package winum
  :straight t
  :config
  (winum-mode)
  (general-def winum-keymap
    "M-0" 'winum-select-window-0-or-10
    "M-1" 'winum-select-window-1
    "M-2" 'winum-select-window-2
    "M-3" 'winum-select-window-3
    "M-4" 'winum-select-window-4
    "M-5" 'winum-select-window-5
    "M-6" 'winum-select-window-6
    "M-7" 'winum-select-window-7
    "M-8" 'winum-select-window-8
    "M-9" 'winum-select-window-9)
  )


(use-package vscode-dark-plus-theme
  :straight t
  :custom
  (vscode-dark-plus-scale-org-faces nil)
  (vscode-dark-plus-invert-hl-todo nil)
  :config
  (load-theme 'vscode-dark-plus t))

;; wgrep
(straight-use-package 'wgrep)

;; Company
;; remember that navigating in the popup is done with M-n and M-p

(use-package company
  :straight t
  :custom
  (company-dabbrev-downcase nil)
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (lsp-completion-provider :capf)
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package diminish
  :straight t)

;; ivy
(use-package ivy
  :straight t
  :init
  (use-package amx
    :straight t
    :defer t)
  (use-package counsel
    :straight t
    :diminish
    :general
    ("M-x" 'counsel-M-x)
    ("C-x C-f" 'counsel-find-file)
    ("C-c j" 'counsel-git-grep)
    :config
    (counsel-mode 1)
    :if (featurep 'helpful)
    :custom
    (counsel-describe-function-function 'helpful-callable)
    (counsel-describe-variable-function 'helpful-variable)
    )
  (use-package swiper
    :general
    ("C-s" 'swiper-isearch)
    ("M-s ." 'swiper-isearch-thing-at-point)
    :defer t)
  :hook
  (after-init . ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  (ivy-display-style nil)
  )


(straight-use-package 'avy)


;; direnv
(use-package direnv
  :straight t
  :config
  (direnv-mode))

;; magit
(use-package magit
  :straight t
  :init
  :general
  (my-leader-def
    "g s" 'magit-status
    "g b" 'magit-blame)
  :config
  (transient-append-suffix 'magit-pull "-A" '("-f" "ff only" "--ff-only")))


(require 'epa-file)
(epa-file-enable)


(use-package parinfer
  :straight t
  :init
  (add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
  (add-hook 'scheme-mode-hook 'parinfer-mode)
  (add-hook 'lisp-mode-hook 'parinfer-mode)
  :general
  (:keymaps 'parinfer-mode-map
	    "C-," 'parinfer-toggle-mode)
  :custom
  (parinfer-extensions '(defaults pretty-parens smart-yank))
  )
;; (use-package parinfer-rust-mode
;;   :straight t
;;   :init
;;   (setq parinfer-rust-auto-download t)
;;   :hook
;;   '(emacs-lisp-mode scheme-mode lisp-mode)
;;   :custom
;;   (parinfer-rust-preferred-mode 'paren)
;;   )


(straight-use-package 'yasnippet)

(use-package quickrun
  :straight t
  :general
  (my-leader-def
    "x" 'quickrun)
  :config
  (quickrun-add-command "arturo"
    '((:command . "c:/Users/Niv/Sync/utility-software/arturo-0.9.78-Windows-full/arturo.exe")
      (:exec    . "%c %s"))
    :mode 'art-mode)

  ;; When file suffix is '.art', then `quickrun' uses "arturo" command-key.
  (add-to-list 'quickrun-file-alist '("\\.art$" . "arturo"))
  (quickrun-add-command "oak"
    '((:command . "c:/Users/Niv/.local/bin/oak")
      (:exec    . "%c %s"))
    :mode 'oak-mode)

  (add-to-list 'quickrun-file-alist '("\\.oak$" . "oak"))
  :general
  (my-leader-def
    "M-c" 'quickrun)
  )

(use-package deadgrep
  :straight t
  :general
  (my-leader-def
    "s" 'deadgrep))

;; YAML
(use-package yaml-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("//.yml//'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("//.yaml//'" . yaml-mode)))

;;; Languages:
(use-package lsp-mode
  :straight t
  :commands lsp
  :hook
  (lsp-mode . (lambda ()
                (let ((lsp-keymap-prefix "C-c l"))
                  (lsp-enable-which-key-integration))))
  (enh-ruby-mode . lsp)
  (ruby-mode . lsp)
  (c-mode . lsp)
  (c++-mode . lsp)
  (python-mode . lsp)
  (json-mode . lsp)
  (js-mode . lsp)
  (jsx-mode . lsp)
  (dart-mode . lsp)
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.6)
  ;; (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-lens-enable t)
  (lsp-signature-auto-activate nil)
  )


(use-package dap-mode
  :straight t
  :init
  ;; Enabling only some features
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (require 'dap-python)
  (require 'dap-cpptools))

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

(use-package alert
  :straight t
  :custom
  (alert-default-style 'libnotify))


;; (straight-use-package
;;  '(lsp-ivy
;;    :type git
;;    :host github
;;    :repo "emacs-lsp/lsp-ivy"
;;    ))
(use-package lsp-ui
  :straight t
  :after (lsp-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 1)
  :hook
  (lsp-mode . lsp-ui-mode))


;; dot (graphviz)
(use-package graphviz-dot-mode
  :straight t
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

;; (use-package company-graphviz-dot
;;   :straight t)


;; lua
(use-package lua-mode
  :straight t
  :custom
  (lua-indent-level 2))

;; haskell
(use-package haskell-mode
  :straight t)

;; ruby
(use-package enh-ruby-mode
  :straight t
  :after (lsp-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'lsp-language-id-configuration '(enh-ruby-mode . "ruby"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("bundle" "exec solargraph stdio"))
		    :major-modes '(ruby-mode enh-ruby-mode)
		    :priority -1
		    :multi-root t
		    :server-id 'ruby-ls))
  )


(customize-set-variable 'lsp-solargraph-use-bundler t)
(use-package pry
  :straight t)


(use-package dart-mode
  :straight t)

(use-package lsp-dart
  :straight t
  :after lsp-mode
  :config
  (setq read-process-output-max (* 1024 1024)))

(use-package flycheck
  :straight t)

(use-package hover
  :straight t
  :after lsp-dart)

;; c-c++
;; (defun c-mode-set-style ()
;;   (setq c-default-style "linux" ; set style to "linux" cause kernel
;; 	tab-width 4		; so we don't overflow lines
;; 	indent-tabs-mode t))	; so we use tabs
;; (setq c-default-style "linux") ; set style to "linux" cause kernel

(setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(c-add-style
 "my-linux-tabs-only"
 '("linux" (c-offsets-alist
	    (arglist-cont-nonempty
	     c-lineup-gcc-asm-reg
	     c-lineup-arglist-tabs-only))
   (c-basic-offset . 4)))

(defun c-mode-set-smartparens ()
  (with-eval-after-load 'smartparens
    (sp-with-modes
	'(c++-mode objc-mode c-mode
		   (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))))

(add-hook 'c-mode-hook
	  (lambda ()
	    (c-set-style "my-linux-tabs-only")
	    (c-mode-set-smartparens)
	    (setq indent-tabs-mode t)
	    (setq tab-width c-basic-offset)))


(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;; rust
(use-package rustic
  :straight t
  ;; :hook
  ;; (rustic-mode . electric-pair-local-mode)
  :general
  (:keymaps 'rustic-mode-map
	    "M-j"  'lsp-ui-imenu
	    "M-?"  'lsp-find-references
	    "C-c C-c l"  'flycheck-list-errors
	    "C-c C-c a"  'lsp-execute-code-action
	    "C-c C-c r"  'lsp-rename
	    "C-c C-c q"  'lsp-workspace-restart
	    "C-c C-c Q"  'lsp-workspace-shutdown
	    "C-c C-c s"  'lsp-rust-analyzer-status)
  :config
  (setq lsp-eldoc-hook nil)
  (setq lsp-rust-analyzer-server-display-inlay-hints t))

(use-package rust-playground
  :straight t)

(use-package pest-mode
  :straight (pest-mode
	     :type git
	     :host github
	     :repo "ksqsf/pest-mode")
  :mode "\\.pest\\'"
  :hook (pest-mode . flymake-mode))

;; raku
(use-package raku-mode
  :straight t)

;; scheme
(straight-use-package 'geiser)

(use-package paredit
  :straight t
  :hook
  (clojure-mode . paredit-mode)
  (emacs-lisp-mode . paredit-mode)
  :general (:keymaps 'paredit-mode-map
		     "M-C b" 'paredit-backward
		     "M-C f" 'paredit-forward
		     "M-C w" 'paredit-backward-up
		     "M-C a" 'paredit-backward-down
		     "M-C s" 'paredit-forward-down
		     "M-C d" 'paredit-forward-up)
  )

;; clojure
(straight-use-package 'clojure-mode)
(straight-use-package 'clojure-mode-extra-font-locking)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'inf-clojure)


(add-hook 'clojure-mode-hook #'subword-mode)
(require 'clojure-mode-extra-font-locking)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

(setq cider-default-repl-command "clojure-cli")
(setq inf-clojure-generic-cmd "clj")


;; python
;;  currently using the default python support which is good enough for now
;; (use-package pyvenv
;;   :straight t
;;   :config
;;   (setq pyvenv-mode-line-indicator
;;         '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
;;   (pyvenv-mode +1))
;; (setq lsp-clients-python-library-directories "~/.local/")
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"))

(use-package auto-virtualenv
  :straight t
  :config
  (require 'auto-virtualenv)
  :hook
  (python-mode . auto-virtualenv-set-virtualenv)
  (focus-in . auto-virtualenv-set-virtualenv)
  (window-configuration-change . auto-virtualenv-set-virtualenv))

(setq lsp-pyls-plugins-flake8-max-line-length 100)

(use-package dockerfile-mode
  :straight t)

;; also maybe:
;; scala
;; elixir
;; java

;; get something as emacs help (`helpful' or something)

;; TODO:
;; Consider Using:
;; - god mode: https://github.com/emacsorphanage/god-mode
;; - f.el: https://github.com/rejeep/f.el
;; - s.el: https://github.com/magnars/s.el
;; - emacs disk usage: https://gitlab.com/ambrevar/emacs-disk-usage
;; - treemacs: https://github.com/Alexander-Miller/treemacs
;; - look for useful stuff in: https://github.com/MatthewZMD/.emacs.d#orgd39ff00


;; fix RET in terminal




;;; reminders on how to use fonts:
;; (seq-map (lambda (font)
;; 	   (let ((info (font-info font)))
;; 	     (if info (aref info 1)
;; 	       nil)))
;; 	 (font-family-list))
;; (font-family-list)
;; (set-face-attribute 'default nil
;; 		    :family "Ubuntu Mono"
;; 		    :height 120)
;; (set-face-attribute 'default nil
;; 		    :family "Hack"
;; 		    :height

;;;actual font settings
(setq my-prefered-font
      (cond ((eq system-type 'windows-nt) "hack")
            ((eq system-type 'gnu/linux) "Noto Sans Mono")
            (t nil)))

(when my-prefered-font
  (set-frame-font my-prefered-font nil t))

(set-face-attribute 'default nil :height 110)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(use-package just-mode
  :straight t)

(use-package justl
  :straight (justl
	     :type git
	     :host github
	     :repo "psibi/justl\.el")
  :custom
  (justl-executable "c:/Users/Niv/.local/scoop/shims/just.exe"))

;; kmonad - kbd mode
(use-package kbd-mode
  :straight (kbd-mode
	     :type git
	     :host github
	     :repo "kmonad/kbd-mode")
  )
