;;; init.el --- Initialization file for Emacs
;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary: Emacs Startup File --- initialization for Emacs

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

(use-package my-mode
  :straight '(my-mode :local-repo "my-mode"))

(use-package general
  :straight t
  :config
  (general-create-definer my-def :keymaps 'my-mode-map)
  (general-create-definer my-leader-def :prefix "C-c" :wrapping my-def)

  (my-leader-def
    "b" (general-key-dispatch (lambda () (interactive) (switch-to-buffer "*scratch*"))
	  :timeout 0.25
	  "b" 'new-empty-buffer)
    "d" 'duplicate-current-line-or-region
    "-" 'split-window-below
    "/" 'split-window-right
    "<backspace>" 'delete-window
    "r" 'rename-file-and-buffer
    "I" 'find-user-init-file
    "<tab>" 'alternate-buffer
    "RET" 'newline-and-indent
    )

  (general-def "C-a" 'prelude-move-beginning-of-line)
  (general-def "M-k" 'kill-whole-line)
  (general-def "M-n" (lambda () (interactive) (scroll-up 1)))
  (general-def "M-p" (lambda () (interactive) (scroll-down 1)))
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
    "e f" 'ediff-files
    "e b" 'ediff-buffers
    "e c" 'ediff-current-file)
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

(use-package elgrep
  :straight t)

(use-package doct
  :straight t
  :commands (doct))


(defconst org-plus-contrib-fixed-recipe
  `(org-plus-contrib
    :type git
    :repo "https://code.orgmode.org/bzg/org-mode.git"
    :local-repo "org"
    :depth full
    :pre-build ,(list
		 (concat
		  (when (eq system-type 'berkeley-unix) "g")
		  "make")
		 "autoloads"
		 (concat "EMACS=" "'" invocation-directory invocation-name "'"))
    :build
    (:not autoloads)
    :files
    (:defaults "lisp/*.el"
	       ("etc/styles/" "etc/styles/*")
	       "contrib/lisp/*.el")
    :includes org))

(straight-override-recipe org-plus-contrib-fixed-recipe)

(use-package org
  :straight org-plus-contrib
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
  (org-agenda-finalize . org-habit-streak-count)
  (org-after-todo-statistics . org-summary-todo)
  (ediff-select . f-ediff-org-unfold-tree-element)
  (ediff-unselect . f-ediff-org-fold-tree)
  :custom
  ;; for more complex stuff look at `org-depend.el'
  (org-enforce-todo-dependencies t "block setting to DONE until previous siblings and children are DONE")
  (org-enforce-todo-checkbox-dependencies t "same as above but for checkboxes")
  (org-cycle-separator-lines 0)
  (org-agenda-start-on-weekday nil)
  (org-agenda-start-day "-1d")
  (org-extend-today-until 2)
  (org-use-effective-time t)
  (org-log-reschedule 'time)
  (org-log-into-drawer t)
  (org-agenda-files (list "~/Sync/organizing/cady-tasks.org"
                          "~/Sync/organizing/MyTasks.org"
                          "~/Sync/organizing/miluim.org"
                          "~/Sync/organizing/important-events.org"
                          "~/Sync/organizing/passerine-tasks.org"))
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-targets '((nil :maxlevel . 3)
			(org-agenda-files :maxlevel . 9)))
  (org-agenda-custom-commands '(("d" "" todo "DELEGATED")
				("c" "" todo "DONE|DEFERRED|CANCELLED")
				("w" "" todo "WAITING")))
  (org-agenda-sorting-strategy '((agenda habit-down category-keep tag-up time-up priority-down )
				 (todo priority-down category-keep)
				 (tags priority-down category-keep)
				 (search category-keep)))

  (org-capture-templates
   (doct '(("Todo"
	    :keys "t"
	    :file "~/Sync/organizing/new-tasks.org"
	    :todo-state "TODO"
	    :template ("* %{todo-state} %^{Description}"
		       ":PROPERTIES:"
		       ":CREATED: %U"
		       ":END:"
		       "%a"
		       "%i%?"))
	   ("Event" :keys "m"
	    :template ("* %^{Who?/What?}"
		       ":PROPERTIES:"
		       ":CREATED: %U"
		       ":END:"
		       "%^{When}T"
		       "%a"
		       "%i%?")
	    :children (("Personal"
			:keys "p"
			:file "~/Sync/organizing/MyTasks.org"
			:olp ("Events"))
		       ("Work"
			:keys "w"
			:file "~/Sync/organizing/cady-tasks.org"
			:olp ("Events")))))))
  :general
  (my-leader-def
    "a" 'org-agenda
    "c" 'org-capture
    "osl" 'org-store-link
    "odl" 'org-insert-last-stored-link
    )
  ("M-m s" 'org-todo)
  ("M-m b" 'org-metaleft)
  ("M-m f" 'org-metaright)
  ("C-M-<return>" 'org-insert-subheading)
  ("M-m t" 'org-insert-structure-template)
  ("C-M-S-<return>" 'org-insert-todo-subheading)
  )

(use-package org-habit
  :straight org-plus-contrib
  :config
  (require 'org-habit)
  :custom
  (org-habit-preceding-days 30))

(use-package org-expiry
  :straight org-plus-contrib
  :after org
  :custom
  (org-expiry-created-property-name "CREATED") ; Name of property when an item is created
  (org-expiry-inactive-timestamps   t)
  :config
  (advice-add 'org-insert-heading :after 'org-expiry-insert-created)
  )

(use-package org-roam
  :straight t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Sync/knowlege-base/")
  (org-roam-completion-system 'ivy)
  :general
  (:keymaps 'org-roam-mode-map
	    "C-c n l" 'org-roam
	    "C-c n f" 'org-roam-find-file
	    "C-c n g" 'org-roam-graph)
  (:keymaps 'org-mode-map
	    "C-c n i" 'org-roam-insert
	    "C-c n I" 'org-roam-insert-immediate))


(setq visible-bell t)

;;use ibuffer instead of list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
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


;; expand region with C-:
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
  :general
  (:keymaps 'undo-tree-map
	    "C-/" nil
	    "C-?" nil
	    )
  (my-def
    "<undo>" 'undo-tree-undo
    "<redo>" 'undo-tree-redo
    "C-z" "<undo>"
    "C-S-z" "<redo>"
    )
  ;;this has to stay in the global map otherwise undo-tree thinks it
  ;;shouldn't be activated, see undo tree documentation for more info
  ("C-/" 'comment-line)
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


(use-package solaire-mode
  :straight t
  :hook
  ;; Ensure solaire-mode is running in all solaire-mode buffers
  (change-major-mode . turn-on-solaire-mode)
  ;; this prevents solaire-mode from turning itself off every time
  ;; Emacs reverts the file
  (after-revert . turn-on-solaire-mode)
  ;; enable solaire-mode unconditionally for certain modes:
  :hook (ediff-prepare-buffer . solaire-mode)
  ;; Highlight the minibuffer when it is activated:
  :hook (minibuffer-setup . solaire-mode-in-minibuffer)
  ;; :custom
  ;; The bright and dark background colors are automatically swapped
  ;; the first time solaire-mode is activated. Namely, the backgrounds
  ;; of the `default` and `solaire-default-face` faces are
  ;; swapped. This is done because the colors are usually the wrong
  ;; way around. If you don't want this, you can disable it:
  ;; (solaire-mode-auto-swap-bg nil)
  :config
  (solaire-global-mode +1))

(use-package vscode-dark-plus-theme
  :straight t
  :after solaire-mode
  :custom
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
    (counsel-mode 1))
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


(use-package forge
  :straight t
  :after magit)


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


(straight-use-package 'yasnippet)


;; YAML
(use-package yaml-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("//.yml//'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("//.yaml//'" . yaml-mode)))

;;; Languages:
(use-package lsp-mode
  :straight t
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
  )

(use-package dap-mode
  :straight t
  :init
  ;; Enabling only some features
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (require 'dap-python))

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
  :after tree-sitter
  )


;; (straight-use-package
;;  '(lsp-ivy
;;    :type git
;;    :host github
;;    :repo "emacs-lsp/lsp-ivy"
;;    ))
(use-package lsp-ui
  :straight t
  :after (lsp-mode)
  :init
  (define-prefix-command 'lsp-ui-doc-map nil "bindings for lsp-ui-doc-functions")
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  :hook
  (lsp-mode . lsp-ui-mode)
  )

;; ([remap xref-find-definitions] . 'lsp-ui-peek-find-definitions)
;; ([remap xref-find-references] . 'lsp-ui-peek-find-references)


;; ;; haskell
;; (straight-use-package 'haskell-mode)
;; (straight-use-package 'company-ghc)
;; (if (bound-and-true-p company-candidates)
;;     (add-to-list 'company-backends 'company-ghc))

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


;; rust
(use-package rustic
  :straight t
  )

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
(use-package pyvenv
  :straight t
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))
;; (setq lsp-clients-python-library-directories "~/.local/")
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
;; - crux: https://github.com/bbatsov/crux
;; - emacs disk usage: https://gitlab.com/ambrevar/emacs-disk-usage
;; - treemacs: https://github.com/Alexander-Miller/treemacs
;; - look for useful stuff in: https://github.com/MatthewZMD/.emacs.d#orgd39ff00


;; fix RET in terminal

(tool-bar-mode -1)


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
            ((eq system-type 'gnu/linux) "mono")
            (t nil)))

(when my-prefered-font
  (set-frame-font my-prefered-font nil t))

(set-face-attribute 'default nil :height 120)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

