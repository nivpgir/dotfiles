(use-package pet
  ;; :ensure-system-package (dasel sqlite3)
  :hook (python-mode . (lambda ()
			 (unless (file-remote-p default-directory)
			   (pet-mode +1))))
  :config
  (defun pet-eglot-setup ()
    "Setup all `eglot' Python language server configuration."
    (with-eval-after-load 'eglot
      (declare-function eglot-alternatives 'eglot)
      (setq-local eglot-server-programs
                  (cons
		   `((python-mode python-ts-mode)
		     . ,(lambda (&optional interactive)
			  (let ((executable-find 'pet-executable-find))
			    (eglot-alternatives
			     (list "pylsp"
                                   "pyls"
                                   "pyright"
                                   "jedi-language-server")))))
                   eglot-server-programs))))

  ;; (add-hook 'python-mode-hook
  ;;           #'(lambda ()
  ;;               ;; Python interpreter
  ;;               (setq-local python-shell-interpreter (pet-executable-find "ipython")
  ;; 			    python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"
  ;;                           python-shell-virtualenv-root (pet-virtualenv-root))
  ;;               ;; Pytest (for python-pytest)
  ;;               (setq-local python-pytest-executable (pet-executable-find "pytest"))
  ;;               ;; Eglot
  ;;               ;; (require 'eglot)
  ;;               ;; (setq-local eglot-server-programs
  ;;               ;;             (cons `((python-mode python-ts-mode)
  ;;               ;;                     . (,(pet-executable-find "pylsp")))
  ;;               ;;                   eglot-server-programs))
  ;;               ))
  :custom
  (pet-toml-to-json-program-arguments '("-f" "-" "-r" "toml" "-w" "json"))
  (pet-yaml-to-json-program-arguments '("-f" "-" "-r" "yaml" "-w" "json")))
(use-package python-pytest)

(use-package python-black)

(use-package python-isort)

(use-package poetry)
(require 'lang-python-eglot)
(use-package emacs
  :straight nil)


