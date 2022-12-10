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
