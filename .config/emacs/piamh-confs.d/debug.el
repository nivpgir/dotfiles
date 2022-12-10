(use-package quickrun
  :straight t
  :general
  (my-leader-def
    "x" 'quickrun)
  :config
  (quickrun-add-command "potion"
    `((:command . ,(executable-find "potion"))
      (:exec    . "%c %s"))
    :mode 'potion-mode)

  ;; When file suffix is '.art', then `quickrun' uses "arturo" command-key.
  (add-to-list 'quickrun-file-alist '("\\.pn$" . "potion"))
  :general
  (my-leader-def
    "M-c" 'quickrun)
  )

