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


(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(use-package straight
             :custom (straight-use-package-by-default t))


(use-package general
  :custom
  (general-use-package-emit-autoloads nil)
  :config
  (general-def "C-z" nil)
  (general-def "C-x C-z" nil)

  (general-create-definer my-def :keymaps 'piamh-mode-map)
  (general-create-definer my-leader-def :prefix "C-c" :wrapping my-def)
  )
