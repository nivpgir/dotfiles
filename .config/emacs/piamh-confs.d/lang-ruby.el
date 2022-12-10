(use-package enh-ruby-mode
  :straight t
  :after (lsp-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
  (add-to-list 'lsp-language-id-configuration '(enh-ruby-mode . "ruby"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("bundle" "exec solargraph stdio"))
		    :major-modes '(ruby-mode enh-ruby-mode)
		    :priority -1
		    :multi-root t
		    :server-id 'ruby-ls))
  )

(customize-set-variable 'lsp-solargraph-use-bundler t)
(use-package pry
  :straight t)

