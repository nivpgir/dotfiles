

;; (use-package piamh-keybindings
;;   :straight '(piamh-keybindings
;; 	      :local-repo "niv-mode"))

(use-package all-the-icons
  :straight t)

(use-package dirvish
  :straight t
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))

  :config
  (dirvish-override-dired-mode)
  (setq dirvish-attributes '(vc-state
			     subtree-state
			     dirvish-peek-mode
			     all-the-icons
			     ;; vs-icons ;; choose one of the icon packs
			     collapse
			     git-msg
			     file-time
			     file-size))
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
  ;;  ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
  ;;  ("^"   . dirvish-history-last)
  ;;  ("h"   . dirvish-history-jump) ; remapped `describe-mode'
  ;;  ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
  ;;  ("M-l" . dirvish-ls-switches-menu)
  ;;  ("M-m" . dirvish-mark-menu)
  ;;  ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
  ;;  ("M-e" . dirvish-emerge-menu)
   ;; ("M-j" . dirvish-fd-jump)
   )
  )

(use-package niv-mode
  :straight '(niv-mode :local-repo "niv-mode")
  :general
  (my-leader-def
    "b" (general-key-dispatch (lambda () (interactive) (switch-to-buffer "*scratch*"))
	  :timeout 0.25
	  "b" 'niv/new-empty-buffer)
    "-" 'split-window-below
    "/" 'split-window-right
    "<backspace>" 'delete-window
    "<tab>" 'niv/alternate-buffer
    "RET" 'newline-and-indent
    "k w" 'delete-window
    "k b" 'kill-buffer
    "k l" 'kill-whole-line
    )

  (general-def "C-a" 'niv/prelude-move-beginning-of-line)
  (general-def "M-k" 'kill-whole-line)
  (general-def "C-x C-e" 'pp-eval-last-sexp)
  (general-def "M-n" (lambda () (interactive) (scroll-up 1)))
  (general-def "M-p" (lambda () (interactive) (scroll-down 1))))

(use-package emacs
  :config
  (tool-bar-mode -1)
  :hook
  (prog-mode . electric-pair-local-mode))

;;; common utils
(use-package crux
  :straight t
  :general
  (my-leader-def
    "n" 'crux-cleanup-buffer-or-region
    "f" 'crux-recentf-find-file
    "e" 'crux-eval-and-replace
    "d" 'crux-duplicate-current-line-or-region
    "M-d" 'crux-duplicate-and-comment-current-line-or-region
    "R" 'crux-rename-file-and-buffer
    "I" 'crux-find-user-init-file
    "," 'crux-find-user-custom-file
    "C-u" 'crux-upcase-region
    "C-l" 'crux-downcase-region
    "M-c" 'crux-capitalize-region
    )
  ("S-RET" 'crux-smart-open-line)
  ("M-o" 'crux-other-window-or-switch-buffer)
  ("C-k" 'crux-kill-and-join-forward)
  )



(use-package diminish
  :straight t)

(use-package direnv
  :straight t
  :config
  (direnv-mode))

(require 'epa-file)
(epa-file-enable)

(use-package deadgrep
  :straight t
  :general
  (my-leader-def
    "s" 'deadgrep))

(use-package alert
  :straight t
  :custom
  (alert-default-style 'libnotify))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
