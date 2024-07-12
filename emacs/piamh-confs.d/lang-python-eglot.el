
;; !!! READ THIS:
;; https://www.adventuresinwhy.com/post/eglot/

;;; Code:
(defun pyrightconfig-write (virtualenv)
  (interactive "DEnv: ")

  (let* (;; file-truename and tramp-file-local-name ensure that neither `~' nor
         ;; the Tramp prefix (e.g. "/ssh:my-host:") wind up in the final
         ;; absolute directory path.
         (venv-dir (tramp-file-local-name (file-truename virtualenv)))

         ;; Given something like /path/to/.venv/, this strips off the trailing `/'.
         (venv-file-name (directory-file-name venv-dir))

         ;; Naming convention for venvPath matches the field for
         ;; pyrightconfig.json.  `file-name-directory' gets us the parent path
         ;; (one above .venv).
         (venvPath (file-name-directory venv-file-name))

         ;; Grabs just the `.venv' off the end of the venv-file-name.
         (venv (file-name-base venv-file-name))

         ;; Eglot demands that `pyrightconfig.json' is in the project root
         ;; folder.

	 (default-directory venvPath)
         (base-dir (vc-root-dir))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))

	 (current-contents (if (file-exists-p out-file)
			       (json-read-file out-file)
			     ()))
	 (current-contents (add-to-list 'current-contents `(venvPath . ,venvPath)))
	 (current-contents (add-to-list 'current-contents `(venv . ,venv)))
	 (current-contents (cl-remove-duplicates current-contents :key 'car :from-end t))
         ;; Finally, get a string with the JSON payload.
	 (out-contents (json-encode current-contents))

         ;; (out-contents (json-encode (list :venvPath venvPath :venv venv)))

    ;; Emacs uses buffers for everything.  This creates a temp buffer, inserts
    ;; the JSON payload, then flushes that content to final `pyrightconfig.json'
    ;; location
	 )
    (with-temp-file out-file
      (insert out-contents)
      (json-pretty-print-buffer)
      )))

(use-package eglot
  :straight nil
  :commands eglot
  )

(use-package flymake-posframe
  :straight (flymake-posframe
	     :type git
	     :host github
	     :repo "Ladicle/flymake-posframe"
	     :files ("*.el" "*.org"))

  :hook (flymake-mode . flymake-posframe-mode))

(use-package lang-python
  :straight eglot
  :hook
  ((python-mode python-ts-mode) . eglot-ensure)
  ((python-mode python-ts-mode) . flyspell-prog-mode)
  ((python-mode python-ts-mode) . superword-mode)
  ((python-mode python-ts-mode) . hs-minor-mode)
  ((python-mode python-ts-mode) . (lambda () (set-fill-column 88)))
  ((python-mode python-ts-mode) . flycheck-mode)
  :config
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :custom
  (python-indent 2)
)

(provide 'lang-python-eglot)
