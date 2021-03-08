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


(column-number-mode)
(setq-default show-trailing-whitespace t)
(require 'tramp)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(global-unset-key (kbd "M-m"))
(global-unset-key (kbd "C-o"))
;; My Functions and configs
(define-prefix-command 'my-keymap nil "niv")
;; (global-set-key (kbd "M-m") 'my-keymap)
(global-set-key (kbd "C-c m") 'my-keymap)
;; (define-key 'my-keymap (kbd "t") (lambda () (interactive) (term "/bin/bash")))
(global-set-key (kbd "M-k") 'kill-whole-line)

(global-auto-revert-mode t)

(defun compose (f g)
  `(lambda (x) (,f (,g x))))
;; setup splitting windows
(define-key 'my-keymap (kbd "-") 'split-window-below)
(define-key 'my-keymap (kbd "/") 'split-window-right)
(define-key 'my-keymap (kbd "<backspace>") 'delete-window)
(define-key 'my-keymap (kbd "e f") 'ediff-files)
(define-key 'my-keymap (kbd "e b") 'ediff-buffers)
(define-key 'my-keymap (kbd "e c") 'ediff-current-file)

(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
(define-key 'my-keymap (kbd "d") 'duplicate-current-line-or-region)
(defun alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))
(define-key 'my-keymap (kbd "<tab>") 'alternate-buffer)

(defun new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer (read-from-minibuffer "New buffer name: "))))
    (switch-to-buffer $buf)
    (setq buffer-offer-save t)
    $buf))


(define-key 'my-keymap (kbd "r") 'rename-file-and-buffer)

(use-package elgrep
  :straight t)

(use-package doct
  :straight t
  :commands (doct))

(use-package org-mode
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

  :custom
  ;; for more complex stuff look at `org-depend.el'
  (org-enforce-todo-dependencies t "block setting to DONE until previous siblings and children are DONE")
  (org-enforce-todo-checkbox-dependencies t "same as above but for checkboxes")
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (org-cycle-separator-lines 0)
  (org-agenda-start-on-weekday nil)
  (org-agenda-start-day nil)
  (org-agenda-files (list "~/Sync/organizing/cady-tasks.org"
                          "~/Sync/organizing/MyTasks.org"
                          "~/Sync/organizing/miluim.org"
                          "~/Sync/organizing/passerine-tasks.org"))
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-targets '((nil :maxlevel . 3)
			(org-agenda-files :maxlevel . 9)))
  (org-agenda-custom-commands '(("d" "" todo "DELEGATED")
				("c" "" todo "DONE|DEFERRED|CANCELLED")
				("w" "" todo "WAITING")))
  (org-capture-templates (doct
			  '(("New TODO"
			     :keys "n"
			     :file "~/Sync/organizing/new-tasks.org"
			     :todo-state "TODO"
			     :template ("* %{todo-state} %^{Description}"
					":PROPERTIES:"
					":CREATED: %U"
					":END:"
					"%i"
					"%a"
					"%?")))))
  ;; ("j" "Journal" entry (file+datetree "~/Sync/organizing/journal-notes.org")
  ;;  "* %?\nEntered on %U\n  %i\n  %a")))
  :hook
  (org-after-todo-statistics . org-summary-todo)
  (ediff-select . f-ediff-org-unfold-tree-element)
  (ediff-unselect . f-ediff-org-fold-tree)
  :bind
  (("C-c a" . org-agenda))
  (("C-c c" . org-capture))
  (("M-m i" . org-insert-item))
  (("M-m f" . org-metaright))
  (("M-m b" . org-metaleft))
  (("M-m t" . org-insert-structure-template))
  (("M-m s" . org-todo))
  )



(use-package org-expiry
  :straight org-plus-contrib
  :init

  (defadvice org-insert-todo-heading (after mrb/created-timestamp-advice activate)
    "Insert a CREATED property using org-expiry.el for TODO entries"
    (org-expiry-insert-created))
  
  (ad-activate 'org-insert-todo-heading)
  :custom
  (org-expiry-created-property-name "CREATED") ; Name of property when an item is created
  (org-expiry-inactive-timestamps   t)
  )


;; go to init.el
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(defun org-time-stamp-no-prompt ()
  (insert (format-time-string
	   (org-time-stamp-format 'long)
	   (org-current-effective-time))))

(defun org-time-stamp-inactive-no-prompt ()
  (insert (format-time-string
	   (org-time-stamp-format 'long 'inactive)
	   (org-current-effective-time))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(define-key 'my-keymap (kbd "I") 'find-user-init-file)
(define-key (current-global-map) (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(define-key (current-global-map) (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(define-key 'my-keymap (kbd "b") 'new-empty-buffer)
(define-key 'my-keymap (kbd "t") 'org-time-stamp-inactive-no-prompt)
(define-key 'my-keymap (kbd "T") 'org-time-stamp-no-prompt)






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

(straight-use-package 'ztree)
(straight-use-package 'smartparens)
(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode)
(setq show-paren-style 'expression)

(straight-use-package 'which-key)
(require 'which-key)
(which-key-mode t)
(which-key-setup-minibuffer)
(setq which-key-popup-type 'minibuffer)




;; expand region with C-:
(straight-use-package 'expand-region)
(define-key (current-global-map) (kbd "M-h") 'er/expand-region)

;; undo-tree with diff on visualizing
(straight-use-package 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-diff t)
;; comment with C-/
;; add a binding to comment-line in the global map and then remove
;; the undo-tree-undo "C-/" binding from undo-tree-map because it
;; takes precedence over the global map
(define-key (current-global-map) (kbd "C-/") 'comment-line)
(define-key undo-tree-map (kbd "C-/") nil) 

(setq winum-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-`") 'winum-select-window-by-number)
        (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
        (define-key map (kbd "M-1") 'winum-select-window-1)
        (define-key map (kbd "M-2") 'winum-select-window-2)
        (define-key map (kbd "M-3") 'winum-select-window-3)
        (define-key map (kbd "M-4") 'winum-select-window-4)
        (define-key map (kbd "M-5") 'winum-select-window-5)
        (define-key map (kbd "M-6") 'winum-select-window-6)
        (define-key map (kbd "M-7") 'winum-select-window-7)
        (define-key map (kbd "M-8") 'winum-select-window-8)
        (define-key map (kbd "M-9") 'winum-select-window-9)
        map))
(straight-use-package 'winum)
(winum-mode)


(use-package nord-theme
  :straight t
  :init
  (load-theme 'nord t))


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
;; (straight-use-package 'ivy)
(use-package ivy
  :straight t
  :init
  (use-package amx
    :straight t
    :defer t)
  (use-package counsel
    :straight t
    :diminish
    :bind
    (("M-x" . counsel-M-x))
    (("C-x C-f" . counsel-find-file))
    (("C-c j" . counsel-git-grep))
    (:map my-keymap
	  ("i u" . counsel-unicode-char))
    :config (counsel-mode 1))
  (use-package swiper
    :defer t)
  :hook (after-init . ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  :bind
  (("C-s" . swiper-isearch))
  (("M-s ." . swiper-isearch-thing-at-point))
  :config
  (setq ivy-display-style nil))


(straight-use-package 'avy)


;; (global-set-key (kbd "C-s") 'swiper-isearch)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "M-s .") 'swiper-isearch-thing-at-point)

;; (define-key 'my-keymap (kbd "h f") 'counsel-describe-function)
;; (define-key 'my-keymap (kbd "h v") 'counsel-describe-variable)
;; (define-key 'my-keymap (kbd "h l") 'counsel-find-library)
;; (define-key 'my-keymap (kbd "h i") 'counsel-info-lookup-symbol)


;; direnv
(use-package direnv
  :straight t
  :config
  (direnv-mode))

;; magit

(use-package magit
  :straight t
  :init
  :bind
  (:map my-keymap
	("g s" . magit-status)
	("g b" . magit-blame))
  :config
  (transient-append-suffix 'magit-pull "-A" '("-f" "ff only" "--ff-only")))


(use-package forge
  :straight t
  :after magit)


(require 'epa-file)
(epa-file-enable)

(straight-use-package 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(use-package parinfer
  :straight t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
	  '(defaults	   ;should be included.
	     pretty-parens ; different paren styles for different modes.
	     smart-yank))
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))


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
  (rust-mode . lsp)
  (python-mode . lsp)
  (js-mode . lsp)
  (jsx-mode . lsp)
  )

(use-package dap-mode
  :straight t
  :init
  ;; Enabling only some features
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (require 'dap-python))

;; indent whole buffer
;; (define-key (current-global-map) (kbd "M-<tab>") 'lsp-format-buffer)


;; (straight-use-package
;;  '(lsp-ivy
;;    :type git
;;    :host github
;;    :repo "emacs-lsp/lsp-ivy"
;;    ))
(use-package lsp-ui
  :straight t
  ;; :after (lsp-mode)
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (define-prefix-command 'lsp-ui-doc-map nil "bindings for lsp-ui-doc-functions")
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  )

;; ([remap xref-find-definitions] . 'lsp-ui-peek-find-definitions)
;; ([remap xref-find-references] . 'lsp-ui-peek-find-references)

(use-package company-lsp
  :straight t)

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


;; (setq lsp-solargraph-use-bundler t)
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
(straight-use-package 'rust-mode)
(add-hook 'rust-mode-hook
    (lambda ()
      (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
(with-eval-after-load 'smartparens
  (sp-with-modes
      '(rust-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))
(straight-use-package 'cargo)
(straight-use-package 'flycheck-rust)
(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
(add-hook 'rust-mode-hook 'flycheck-mode)


;; scheme
(straight-use-package 'geiser)

;; clojure
(straight-use-package 'paredit)
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

(global-set-key (kbd "RET") 'newline-and-indent)

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


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

