(use-package quickrun
  :straight t
  :custom
  (quickrun-timeout-seconds nil)
  :config

  (defun run-pytest-with-uv ()
  "Run pytest in current project root using uv."
  (interactive)
  (let ((project-root (locate-dominating-file (buffer-file-name) "pyproject.toml")))
    (if project-root
        (quickrun
         :command "uv run pytest"
         :dir project-root)
      (message "No pyproject.toml found in project root"))))

  (quickrun-add-command
    "python/pytest"
    `((:command . (lambda ()
		    (interactive)
		    (let ((testfile (tramp-file-local-name
				     (read-file-name-default "Test file: "))))
		      (concat "uv run pytest " testfile)))
		))
    :mode 'python-mode)

  (quickrun-add-command
    "potion"
    `((:command . ,(executable-find "potion"))
      (:exec    . "%c %s"))
    :mode 'potion-mode)
  (add-to-list 'quickrun-file-alist '("\\.pn$" . "potion"))

  ;; When file suffix is '.pn', then `quickrun' uses "potion" command-key.
  :general
  ("C-M-S-<return>" 'quickrun)
  (:keymaps 'popper-mode-map
	    "C-<escape>" 'popper-close-popup-window
	    )
  )

