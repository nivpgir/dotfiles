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
    "A m" 'gptel-menu
    "A s" 'gptel-send
    "A a" 'gptel-abort
    "A r" 'gptel-rewrite
    )
  )
