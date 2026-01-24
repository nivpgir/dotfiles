;;; llm.el --- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package gptel
  ;; :custom
  ;; (gptel-model   'local)
  ;; (gptel-backend (gptel-make-openai "llama-cpp"
  ;;                  :stream t
  ;;                  :protocol "http"
  ;;                  :host "localhost:8080"
  ;;                  :models '(local)))
  :general
  (my-leader-def
    "l m" 'gptel-menu
    "l s" 'gptel-send
    "l a" 'gptel-abort
    "l r" 'gptel-rewrite
    )
  )

(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ("C-c c" . claude-code-command-map) ;; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode))
