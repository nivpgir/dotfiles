(use-package company
  :straight t
  :custom
  (company-dabbrev-downcase nil)
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (lsp-completion-provider :capf)
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package ivy
  :straight t
  :init
  (use-package amx
    :straight t
    :defer t)
  (use-package counsel
    :straight t
    :diminish
    :general
    ("M-x" 'counsel-M-x)
    ("C-x C-f" 'counsel-find-file)
    ("C-c j" 'counsel-git-grep)
    :config
    (counsel-mode 1)
    :if (featurep 'helpful)
    :custom
    (counsel-describe-function-function 'helpful-callable)
    (counsel-describe-variable-function 'helpful-variable)
    )
  (use-package swiper
    :general
    ("C-s" 'swiper-isearch)
    ("M-s ." 'swiper-isearch-thing-at-point)
    :defer t)
  :hook
  (after-init . ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  (ivy-display-style nil)
  )

(straight-use-package 'avy)
