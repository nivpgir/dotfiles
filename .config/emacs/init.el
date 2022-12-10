;; init.el --- Initialization file for Emacs
;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary: Emacs Startup File --- initialization for Emacs

(add-to-list 'load-path (expand-file-name "piamh-confs.d" user-emacs-directory))
(load "straight-setup.el")

(load "misc.el")
;; (load "keybinding-utils.el")
(load "editing.el")
(load "autocompletion.el")
(load "debug.el")
(load "window-management.el")
(load "theme.el")
;; (load "org-conf.el")
(load "prog-lang-env.el")
(load "theme.el")
(load "version-control.el")
(load "lang-elisp.el")
(load "lang-c-cpp.el")
(load "lang-clojure.el")
(load "lang-dart.el")
(load "lang-dockerfile.el")
(load "lang-graphviz.el")
(load "lang-haskell.el")
(load "lang-just.el")
(load "lang-kmonad-kbd.el")
(load "lang-lua.el")
(load "lang-potion.el")
(load "lang-python.el")
(load "lang-raku.el")
(load "lang-ruby.el")
(load "lang-rust.el")
(load "lang-scheme.el")
(load "lang-yaml.el")


;; Company
;; remember that navigating in the popup is done with M-n and M-p





;; ivy






;; direnv


;; magit










;; YAML



;;; Languages:














;; (straight-use-package
;;  '(lsp-ivy
;;    :type git
;;    :host github
;;    :repo "emacs-lsp/lsp-ivy"
;;    ))



;; dot (graphviz)


;; (use-package company-graphviz-dot
;;   :straight t)


;; lua


;; haskell


;; ruby
















;; rust


;; raku


;; scheme




;; clojure


;; python
;;  currently using the default python support which is good enough for now
;; (use-package pyvenv
;;   :straight t
;;   :config
;;   (setq pyvenv-mode-line-indicator
;;         '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
;;   (pyvenv-mode +1))
;; (setq lsp-clients-python-library-directories "~/.local/")



;; also maybe:
;; scala
;; elixir
;; java


;; TODO:
;; Consider Using:
;; - god mode: https://github.com/emacsorphanage/god-mode
;; - f.el: https://github.com/rejeep/f.el
;; - s.el: https://github.com/magnars/s.el
;; - emacs disk usage: https://gitlab.com/ambrevar/emacs-disk-usage
;; - treemacs: https://github.com/Alexander-Miller/treemacs
;; - look for useful stuff in: https://github.com/MatthewZMD/.emacs.d#orgd39ff00


;; fix RET in terminal







;; kmonad - kbd mode


;; potion

