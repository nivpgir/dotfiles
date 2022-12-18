(use-package lsp-mode
  :straight t
  :commands lsp
  :hook
  (lsp-mode . (lambda ()
                (let ((lsp-keymap-prefix "C-c l"))
                  (lsp-enable-which-key-integration))))
  (enh-ruby-mode . lsp)
  (ruby-mode . lsp)
  (c-mode . lsp)
  (c++-mode . lsp)
  (python-mode . lsp)
  (json-mode . lsp)
  (js-mode . lsp)
  (jsx-mode . lsp)
  (dart-mode . lsp)
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.6)
  ;; (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-lens-enable t)
  (lsp-signature-auto-activate nil)
  )

(use-package lsp-ui
  :straight t
  :after (lsp-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 1)
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package dap-mode
  :straight t
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode) (python-mode . dap-mode))
  :general
  (:keymaps 'lsp-command-map
	    "d" 'dap-hydra)
  :config
  ;; Enabling only some features
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (require 'dap-python)
  (require 'dap-cpptools)
  :custom
  (dap-python-debugger 'debugpy)
  (defun executable-find-with-venv (pyenv-executable-find command)
    (when-let ((command-in-venv (with-venv (executable-find command))))
      (pyenv-executable-find command)))
  (advice-add 'venv-executable-find :around dap-python--pyenv-executable-find)


  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  ;; (dap-gdb-lldb-setup)
  ;; (require 'dap-lldb)
  ;; (require 'dap-gdb-lldb)
  ;; (dap-register-debug-template "Rust::GDB Run Configuration"
  ;;                              (list :type "lldb"
  ;;                                    :request "launch"
  ;;                                    :name "GDB::Run"
  ;; 				     :gdbpath "rust-lldb"
  ;; 				     :target nil
  ;; 				     :cwd nil))
  )

(use-package tree-sitter
  :straight t
  :hook
  ((c-mode-hook
    c++-mode-hook
    css-mode-hook
    html-mode-hook
    js-mode-hook
    js2-mode-hook
    python-mode-hook
    ruby-mode-hook
    rust-mode-hook
    typescript-mode-hook
    python-mode
    rustic-mode) . tree-sitter-hl-mode)
  )

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)


(use-package flycheck
  :straight t)

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))
