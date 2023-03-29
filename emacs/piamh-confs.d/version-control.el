(use-package magit
  :straight t
  :init
  :general
  (my-leader-def
    "g s" 'magit-status
    "g b" 'magit-blame)
  :config
  (transient-append-suffix 'magit-pull "-A" '("-f" "ff only" "--ff-only")))
