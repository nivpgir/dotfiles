(use-package with-venv
  :straight t)

;; (when (executable-find "ipython")
;;   (setq python-shell-interpreter "ipython"
;; 	python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"))

;; (use-package auto-virtualenv
;;   :straight t
;;   :config
;;   (require 'auto-virtualenv)
;;   :hook
;;   (python-mode . auto-virtualenv-set-virtualenv)
;;   (focus-in . auto-virtualenv-set-virtualenv)
;;   (window-configuration-change . auto-virtualenv-set-virtualenv))

;; (setq lsp-pyls-plugins-flake8-max-line-length 100)

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

(use-package eglot
  :straight nil
  :commands eglot
  )

(use-package lang-python
  :straight eglot
  :init
  (defun my/eglot-python-workspace-config (server)
    (let ((mode (eglot-server-mode server))
	  (server (eglot-server-name server)))
      (cond
       ((equal mode "python")
	(cond
	 ((equal server "pylsp")
	  (let ((venv (my/locate-python-venv default-directory)))
	    (list (cons :python
			(list
			 :venvPath venv
			 :pythonPath (expand-file-name "bin/python" venv)))))
	  ))))))

  (defun set-python-venv (&optional venv-dir)
    ;; (setq-local python-shell-virtualenv-root default-directory)
    (let ((venv-dir (or venv-dir (my/locate-python-venv default-directory))))
      (setq-local python-shell-virtualenv-root venv-dir))
    )
  :hook
  ;; (python-mode . (lambda () (let ((eglot-workspace-config
  ;; 				   'my/eglot-python-workspace-config))
  ;; 			      (eglot-ensure))))
  (python-mode . set-python-venv)
  (python-mode . eglot-ensure))

(defun eglot-workspace-config (server)
  (let ((mode (eglot-server-mode server))))
  (server (eglot-server-name server)
	  (cond
	   ((equal mode "python")
	    (cond
	     ((equal server "pylsp"))))))
  (let ((venv (my/locate-python-venv default-directory)))
    (list (cons :python)
          (list
           :venvPath venv
           :pythonPath (expand-file-name "bin/python" venv)))))
