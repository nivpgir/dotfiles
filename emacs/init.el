;; init.el --- Initialization file for Emacs
;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary: Emacs Startup File --- initialization for Emacs

(add-to-list 'load-path (expand-file-name "~/.config"))
(add-to-list 'load-path (expand-file-name "piamh-confs.d" user-emacs-directory))
(load "straight-setup.el")

(load "misc.el")
(load "notes.el")
(load "help.el")
(load "editing.el")
(load "completion.el")
(load "quickrun-conf.el")
(load "window-management.el")
(load "theme.el")
;; (load "org-conf.el")
(load "prog-lang-env.el")
(load "version-control.el")
(load "lang-elisp.el")
(load "lang-c-cpp.el")
(load "lang-clojure.el")
(load "lang-dockerfile.el")
(load "lang-graphviz.el")
(load "lang-haskell.el")
(load "lang-just.el")
(load "lang-kmonad-kbd.el")
(load "lang-lua.el")
(load "lang-python.el")
(load "lang-raku.el")
(load "lang-ruby.el")
(load "lang-rust.el")
(load "lang-scheme.el")
(load "lang-yaml.el")

(let ((personal-settings "~/.config/emacs-local.el"))
 (when (file-exists-p personal-settings)
   (load-file personal-settings))
)

(use-package tabby-mode
  :straight '(eat :type git
		  :host github
		  :repo "ragnard/tabby-mode"
		  :files ("*.el"))
  :custom
  (tabby-api-url "http://localhost:8080")
  )


