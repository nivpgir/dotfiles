(use-package with-venv
  :straight t)

(defun my/run-command-in-directory (dir cmd &rest args)
  "Run a command in the specified directory. If the directory is nil, the directory of the file is used. The stdout result is trimmed of whitespace and returned."
  (let (
	(default-directory (or dir default-directory))
	(stdout-buffer (generate-new-buffer "tmp-stdout" t))
	(full-cmd (append '(call-process cmd nil (list stdout-buffer nil) nil) args)))

    (unwind-protect
	(let ((exit-status (condition-case nil (eval full-cmd) (file-missing nil))))
	  (if (eq exit-status 0)
	      (progn
		(with-current-buffer stdout-buffer
		  (string-trim (buffer-string))))))

      (kill-buffer stdout-buffer))))

(defun my/locate-venv-poetry (project-dir)
  "Find a poetry venv."
  (my/run-command-in-directory project-dir "poetry" "env" "info" "-p"))

(defun my/locate-python-venv (project-dir)
  "Find a poetry venv."
  (if-let ((poetry-bin (executable-find "poetry")))
      (my/locate-venv-poetry project-dir)
    (concat project-dir ".venv"))
  )



(use-package lsp-mode
  :config
  ;; (hash-table-keys lsp-clients)
  ;; (gethash 'pylsp lsp-clients)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
		    :activation-fn (lsp-activate-on "python")
		    :priority -1
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pylsp-remote
		    ;; :library-folders-fn (lambda (_workspace) lsp-clients-pylsp-library-directories)
		    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
					(lsp--set-configuration (lsp-configuration-section "pylsp"))))
		    )))

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package treemacs)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
;; (use-package dap-python)

(provide 'lang-python-lsp-mode)
