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
;; (defvar bootstrap-version)
;; (let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
;;       (bootstrap-version 2))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/master/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)


(column-number-mode)
(setq-default show-trailing-whitespace t)
(require 'tramp)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; My Functions and configs
(define-prefix-command 'my-keymap nil "niv")
(global-set-key (kbd "M-m") 'my-keymap)
(define-key 'my-keymap (kbd "t") (lambda () (interactive) (term "/bin/bash")))

(defun compose (f g)
  `(lambda (x) (,f (,g x))))
;; setup splitting windows
(define-key 'my-keymap (kbd "-") 'split-window-below)
(define-key 'my-keymap (kbd "/") 'split-window-right)
(define-key 'my-keymap (kbd "w <backspace>") 'delete-window)
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
    $buf
    ))
;; indent whole buffer
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(define-key (current-global-map) (kbd "M-<tab>") 'indent-buffer)

;; go to init.el
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))
(define-key 'my-keymap (kbd "I") 'find-user-init-file)
(define-key (current-global-map) (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(define-key (current-global-map) (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(defun insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(define-key 'my-keymap (kbd "c") 'compile)
(define-key 'my-keymap (kbd "nb") 'new-empty-buffer)

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

(define-key 'my-keymap (kbd "r") 'rename-file-and-buffer)



;;unbinding C-m from RET
;; (define-key input-decode-map [?\C-m] [C-m]) ;; without this we can't RET doesn't work in terminal
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
(global-set-key (kbd "C-;") 'er/expand-region)
;; undo-tree with diff on visualizing
(straight-use-package 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-diff t)
;; comment with C-/
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-x C-;"))
(global-set-key (kbd "C-/") 'comment-line)
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

;; doom themes
(use-package doom-themes
  :straight t
  :config
  ;; flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (load-theme 'doom-molokai t))


;; doom modeline
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-minor-modes t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-height 15)
  )


;; wgrep
(straight-use-package 'wgrep)

;; Company
;; remember that navigating in the popup is done with M-n and M-p
(straight-use-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

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
    :config (counsel-mode 1))
  (use-package swiper
    :defer t)
  :hook (after-init . ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  :config
  (setq ivy-display-style nil)
  )
  ;; :map
;; (global-set-key (kbd "RET") 'ivy-alt-done)
;; (global-set-key (kbd "<escape>") 'minibuffer-keyboard-quit)
;; (global-set-key (kbd "C-M-s") 'swiper-query-replace)

(straight-use-package 'avy)

;; (straight-use-package 'counsel)


(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "M-m M-s") 'swiper-isearch-thing-at-point)
(define-key 'my-keymap (kbd "M-s") 'swiper-isearch-thing-at-point)
;; (global-set-key (kbd "M-m h f") 'counsel-describe-function)
(define-key 'my-keymap (kbd "h f") 'counsel-describe-function)
;; (global-set-key (kbd "M-m h v") 'counsel-describe-variable)
(define-key 'my-keymap (kbd "h v") 'counsel-describe-variable)
;; (global-set-key (kbd "M-m h l") 'counsel-find-library)
(define-key 'my-keymap (kbd "h l") 'counsel-find-library)
;; (global-set-key (kbd "M-m h i") 'counsel-info-lookup-symbol)
(define-key 'my-keymap (kbd "h i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "M-m i u") 'counsel-unicode-char)
(define-key 'my-keymap (kbd "i u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c c") 'counsel-compile)
(define-key 'my-keymap (kbd "c") 'counsel-compile)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)



;; magit
(straight-use-package 'magit)
(require 'magit)
(define-key 'my-keymap (kbd "g s") 'magit-status)
(define-key 'my-keymap (kbd "g b") 'magit-blame)
;;(straight-use-package 'magithub)


(straight-use-package 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))




(straight-use-package 'yasnippet)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(straight-use-package 'company-lsp)
;; (straight-use-package
;;  '(lsp-ivy
;;    :type git
;;    :host github
;;    :repo "emacs-lsp/lsp-ivy"
;;    ))

;;; Languages:
;; ;; haskell
;; (straight-use-package 'haskell-mode)
;; (straight-use-package 'company-ghc)
;; (if (bound-and-true-p company-candidates)
;;     (add-to-list 'company-backends 'company-ghc))

;; ruby
(add-hook 'ruby-mode-hook 'lsp)
;; (setq lsp-solargraph-use-bundler t)
;; (straight-use-package 'robe)
;; (add-hook 'ruby-mode-hook 'robe-mode)
;; (if (bound-and-true-p company-candidates)
;;     (add-to-list 'company-backends 'company-robe))
(with-eval-after-load 'smartparens
  (sp-with-modes
      '(ruby-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

;; c-c++
;; (defun c-mode-set-style ()
;;   (setq c-default-style "linux" ; set style to "linux" cause kernel
;; 	tab-width 4		; so we don't overflow lines
;; 	indent-tabs-mode t))	; so we use tabs
;; (setq c-default-style "linux") ; set style to "linux" cause kernel
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)


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
        '(c++-mode objc-mode c-mode)
      (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))))

(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "my-linux-tabs-only")
            (c-mode-set-smartparens)
	    (setq indent-tabs-mode t)
	    (setq tab-width c-basic-offset)
            ))



;; dts
(straight-use-package 'dts-mode)

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
(add-hook 'rust-mode-hook 'lsp)
(straight-use-package 'flycheck-rust)
(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
(add-hook 'rust-mode-hook 'flycheck-mode)


;; lua
(straight-use-package 'lua-mode)

;; scheme
(straight-use-package 'geiser)

;; clojure
(dolist (p '(paredit
	     clojure-mode
	     clojure-mode-extra-font-locking
	     rainbow-delimiters
	     inf-clojure
	     cider))
  (straight-use-package p))


(add-hook 'clojure-mode-hook #'subword-mode)
(require 'clojure-mode-extra-font-locking)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(setq cider-default-repl-command "clojure-cli")
(setq inf-clojure-generic-cmd "clj")


;; python
;;  currently using the default python support which is good enough for now
(add-hook 'python-mode-hook 'lsp)

;; purescript
(use-package purescript-mode
  :straight t
  :hook (purescript-mode . turn-on-purescript-indent)
  )
;; (add-hook 'purescript-mode-hook 'turn-on-purescript-indent)


;; also maybe:
;; scala
;; elixir
;; nim
;; java

;; get something as emacs help (helpful or something)

;; TODO:
;; Consider Using:
;; - crux: https://github.com/bbatsov/crux
;; - emacs disk usage: https://gitlab.com/ambrevar/emacs-disk-usage
;; - treemacs: https://github.com/Alexander-Miller/treemacs
;; - look for useful stuff in: https://github.com/MatthewZMD/.emacs.d#orgd39ff00

(global-set-key (kbd "RET") 'newline-and-indent)

;; fix RET in terminal

(tool-bar-mode -1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("02591317120fb1d02f8eb4ad48831823a7926113fa9ecfb5a59742420de206e0" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
