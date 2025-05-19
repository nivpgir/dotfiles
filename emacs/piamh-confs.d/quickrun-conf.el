(use-package quickrun
  :straight t
  :custom
  (quickrun-timeout-seconds nil)
  :config
  (quickrun-add-command "potion"
    `((:command . ,(executable-find "potion"))
      (:exec    . "%c %s"))
    :mode 'potion-mode)

  ;; When file suffix is '.pn', then `quickrun' uses "potion" command-key.
  (add-to-list 'quickrun-file-alist '("\\.pn$" . "potion"))
  :general
  ("C-M-S-<return>" 'quickrun)
  (:keymaps 'popper-mode-map
	    "C-<escape>" 'popper-close-popup-window
	    )
  )

