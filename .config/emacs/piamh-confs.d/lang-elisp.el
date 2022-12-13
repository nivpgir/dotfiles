(use-package elisp-mode
  :straight emacs
  :general
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
	    "C-M-i" nil))

(use-package eros
  :straight t
  :config
  (eros-mode 1)
  :general
  ("C-c C-e" 'eros-eval-last-sexp))


(use-package lispy
  :straight t)

(use-package paredit
  :straight t
  :diminish
  :hook
  (emacs-lisp-mode . paredit-mode)
  :general (:keymaps 'paredit-mode-map
		     "M-C b" 'paredit-backward
		     "M-C f" 'paredit-forward
		     "M-C w" 'paredit-backward-up
		     "M-C a" 'paredit-backward-down
		     "M-C s" 'paredit-forward-down
		     "M-C d" 'paredit-forward-up)
  )

(use-package eldoc
  :straight nil
  :diminish)
