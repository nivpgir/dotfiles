(use-package eat
  :custom
  (eat-term-scrollback-size nil)
  :bind
  (:map eat-semi-char-mode-map
	("M-i" . piamh/switch-or-manage-windows)
	("M-I" . piamh/manage-windows))
  :hook
  (eat-mode . (lambda () (setq-local show-trailing-whitespace nil)))
  :init
  (defun piamh/run-command-with-eat (command &rest args &key startfile)
    (interactive (split-string-shell-command
		  (read-from-minibuffer "Exec command: ")))
    (let* ((name (string-join `("*" ,default-directory " :: " ,command ,@args "*") " "))
	   (buffer (apply 'eat-make `(,name ,command ,startfile ,@args))))
      (message "running cmd: %S, args: %S" command args)
      (with-current-buffer buffer
	(eat-emacs-mode)
	(setq-local show-trailing-whitespace nil)
	)
      (display-buffer buffer 'display-buffer-in-child-frame))
    )
  )

(provide 'piamh-confs/terminal-emulation)

