;; init.el --- Initialization file for Emacs
;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary: Emacs Startup File --- initialization for Emacs

(add-to-list 'load-path (expand-file-name "~/.config"))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (add-to-list 'load-path (expand-file-name user-emacs-directory))

(require 'piamh-confs/straight-setup)

(setq piamh-confs-features
      '(misc
        terminal-emulation
        notes
        help
        editing
        completion
        quickrun-conf
        window-management
        theme
        ;; org-conf
        prog-lang-env
        version-control
        lang-elisp
        lang-c-cpp
        lang-clojure
        lang-dockerfile
        lang-graphviz
        lang-haskell
        lang-just
        lang-kmonad-kbd
        lang-lua
        lang-python
        lang-raku
        lang-ruby
        lang-rust
        lang-scheme
        lang-yaml
        llm
        ;; lang-jenkinsfile
        ))

(dolist (conf piamh-confs-features)
  (require (intern (format "piamh-confs/%s" conf))))

(let ((personal-settings "~/.config/emacs-local.el"))
 (when (file-exists-p personal-settings)
   (load-file personal-settings))
)
;; Youre an assistant to a a python developer that uses emacs as a text editor.
;; give precise, and concise answers.
;; provide short code examples when asked.
;; explain code examples only when asked and do so shortly.
