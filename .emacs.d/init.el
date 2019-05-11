;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs
(defvar bootstrap-version)
(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/master/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(require 'tramp)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; My Functions and configs
(define-prefix-command 'my-keymap nil "niv")
(global-set-key (kbd "M-m") 'my-keymap)
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
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))
(define-key 'my-keymap (kbd "d") 'duplicate-line)
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
;; centered point ;; if this will be problematic then consider using: https://www.emacswiki.org/emacs/centered-cursor-mode.el
;;(straight-use-package
;; '(centered-point-mode :type git :host github :repo "jmercouris/emacs-centered-point"))
;;(require 'centered-point-mode-autoloads)
;;(require 'centered-point-mode)
(defun line-change ()
  (recenter)
  )
(define-minor-mode centered-point-mode
  "Alaways center the cursor in the middle of the screen."
  :lighter "..."
  (cond (centered-point-mode (add-hook 'post-command-hook 'line-change))
	(t (remove-hook 'post-command-hook 'line-change)))
  )
(centered-point-mode t)
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
;; darcula theme
(straight-use-package 'idea-darkula-theme)
(load-theme 'idea-darkula t)
(custom-theme-set-faces
 'idea-darkula
 '(show-paren-match ((t (:background "dark slate gray")))))


;; Company
;; remember that navigating in the popup is done with M-n and M-p
(straight-use-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; ivy
(straight-use-package 'ivy)
(straight-use-package 'swiper)
(straight-use-package 'counsel)
(ivy-mode 1)

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-m h f") 'counsel-describe-function)
(global-set-key (kbd "M-m h v") 'counsel-describe-variable)
(global-set-key (kbd "M-m h l") 'counsel-find-library)
(global-set-key (kbd "M-m h i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "M-m i u") 'counsel-unicode-char)

(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)



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

;; magit
(straight-use-package 'magit)
(require 'magit)
(define-key 'my-keymap (kbd "g s") 'magit-status)
;;(straight-use-package 'magithub)


(straight-use-package 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))



;;; Languages:
;; haskell
(straight-use-package 'haskell-mode)
(straight-use-package 'company-ghc)
(if (bound-and-true-p company-candidates)
    (add-to-list 'company-backends 'company-ghc))

;; ruby
(straight-use-package 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(if (bound-and-true-p company-candidates)
    (add-to-list 'company-backends 'company-robe))
(with-eval-after-load 'smartparens
  (sp-with-modes
      '(ruby-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

;; c-c++
(setq c-default-style "gnu") ;; set style to "linux"
(with-eval-after-load 'smartparens
  (sp-with-modes
      '(c++-mode objc-mode c-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

;; dts
(straight-use-package 'dts-mode)

;; racket
(straight-use-package 'racket-mode)

;; rust
(straight-use-package 'rust-mode)
(straight-use-package 'cargo)
(straight-use-package 'flycheck-rust)
(straight-use-package 'racer)
(setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
(setq racer-rust-src-path "~/local/gits/rust/src") ;; Rust source code PATH
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
(with-eval-after-load 'smartparens
  (sp-with-modes
      '(rust-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))


;; python
;; also maybe:
;; scala
;; elixir
;; nim
;; java

;; get something as emacs help (helpful or something)

(global-set-key (kbd "RET") 'newline-and-indent)

;; fix RET in terminal

(tool-bar-mode -1)


