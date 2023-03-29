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
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
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
;; if you are ivy user
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package treemacs)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
;; (use-package dap-python)


;; (defun lsp-tramp-connection-over-ssh-port-forwarding (command)
;;   "Like lsp-tcp-connection, but uses SSH portforwarding."
;;   (list
;;    :connect (lambda (filter sentinel name environment-fn)
;;               (let* ((host "localhost")
;;                      (lsp-port (lsp--find-available-port host (cl-incf lsp--tcp-port)))
;;                      (command (with-parsed-tramp-file-name buffer-file-name nil
;;                                 (message "[tcp/ssh hack] running LSP %s on %s / %s" command host localname)
;;                                 (let* ((unix-socket (format "/tmp/lsp-ssh-portforward-%s.sock" lsp-port))
;;                                        (command (list
;;                                                  "ssh"
;;                                                  ;; "-vvv"
;;                                                  "-L" (format "%s:%s" lsp-port unix-socket)
;;                                                  host
;;                                                  "socat"
;;                                                  (format "unix-listen:%s" unix-socket)
;;                                                  (format "system:'\"cd %s && %s\"'" (file-name-directory localname) command)
;;                                                  )))
;;                                   (message "using local command %s" command)
;;                                   command)))
;;                      (final-command (if (consp command) command (list command)))
;;                      (_ (unless (executable-find (cl-first final-command))
;;                           (user-error (format "Couldn't find executable %s" (cl-first final-command)))))
;;                      (process-environment
;;                       (lsp--compute-process-environment environment-fn))
;;                      (proc (make-process :name name :connection-type 'pipe :coding 'no-conversion
;;                                          :command final-command :sentinel sentinel :stderr (format "*%s::stderr*" name) :noquery t))
;;                      (tcp-proc (progn
;;                                  (sleep-for 1) ; prevent a connection before SSH has run socat. Ugh.
;;                                  (lsp--open-network-stream host lsp-port (concat name "::tcp")))))

;;                 ;; TODO: Same :noquery issue (see above)
;;                 (set-process-query-on-exit-flag proc nil)
;;                 (set-process-query-on-exit-flag tcp-proc nil)
;;                 (set-process-filter tcp-proc filter)
;;                 (cons tcp-proc proc)))
;;    :test? (lambda () t)))
(provide 'lang-python-lsp-mode)
