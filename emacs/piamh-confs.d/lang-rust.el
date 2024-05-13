;; (use-package rustic
;;   :straight t
;;   ;; :hook
;;   ;; (rustic-mode . electric-pair-local-mode)
;;   :general
;;   (:keymaps 'rustic-mode-map
;; 	    "M-j"  'lsp-ui-imenu
;; 	    "M-?"  'lsp-find-references
;; 	    "C-c C-c l"  'flycheck-list-errors
;; 	    "C-c C-c a"  'lsp-execute-code-action
;; 	    "C-c C-c r"  'lsp-rename
;; 	    "C-c C-c q"  'lsp-workspace-restart
;; 	    "C-c C-c Q"  'lsp-workspace-shutdown
;; 	    "C-c C-c s"  'lsp-rust-analyzer-status)
;;   :config
;;   (setq lsp-eldoc-hook nil)
;;   (setq lsp-rust-analyzer-server-display-inlay-hints t))

(use-package lsp-mode)

(use-package rustic
  :custom
  (rustic-kill-buffer-and-window t)
  )
(use-package rust-playground
  :straight t)

(use-package pest-mode
  :straight (pest-mode
	     :type git
	     :host github
	     :repo "ksqsf/pest-mode")
  :mode "\\.pest\\'"
  :hook (pest-mode . flymake-mode))


