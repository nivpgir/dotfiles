;;; llm.el --- -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package gptel
  :custom
  (gptel-model   'test)
  (gptel-backend (gptel-make-openai "llama-cpp"
                   :stream t
                   :protocol "http"
                   :host "localhost:8080"
                   :models '(test)))
  :general
  (my-leader-def
    "l m" 'gptel-menu
    "l s" 'gptel-send
    "l a" 'gptel-abort
    "l r" 'gptel-rewrite
    )
  )
