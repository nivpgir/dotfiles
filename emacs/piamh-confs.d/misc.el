(use-package dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
	 :map dired-mode-map
	 ([remap dired-do-async-shell-command] . dwim-shell-command)
	 ([remap dired-do-shell-command] . dwim-shell-command)
	 ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  (require 'dwim-shell-commands)
  (defun my/dwim-shell-command-convert-to-gif ()
    "Convert all marked videos to optimized gif(s)."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert to gif"
     "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif"
     :utils "ffmpeg")))

(use-package all-the-icons)

(use-package dirvish
  :straight t
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")
     ))

  :config
  (dirvish-override-dired-mode)
  (dirvish-peek-mode)
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
  :general
  (my-leader-def
    "O" 'dirvish
    "a" 'dirvish-quick-access)
  (:keymaps 'dirvish-mode-map
	    "a" 'dirvish-quick-access
	    "y" 'dirvish-yank-menu
	    "N" 'dirvish-narrow
	    "v" 'dirvish-vc-menu      ; remapped `dired-view-file'
	    "TAB" 'dirvish-subtree-toggle
	    "M-f" 'dirvish-history-go-forward
	    "M-b" 'dirvish-history-go-backward
	    "M-s" 'dirvish-setup-menu
	    "s"   'dirvish-quicksort    ; remapped `dired-sort-toggle-or-edit'
	    "M-t" 'dirvish-layout-toggle
	    ;;  ("f"   . dirvish-file-info-menu)
	    ;;  ("^"   . dirvish-history-last)
	    ;;  ("h"   . dirvish-history-jump) ; remapped `describe-mode'
	    ;;  ("M-l" . dirvish-ls-switches-menu)
	    ;;  ("M-m" . dirvish-mark-menu)
	    ;;  ("M-e" . dirvish-emerge-menu)
	    ;; ("M-j" . dirvish-fd-jump)
	    )
  )



(use-package tramp
  :straight nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; Enable full-featured Dirvish over TRAMP on certain connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
  (add-to-list 'tramp-connection-properties
	       (list (regexp-quote "/ssh:root@nautilus-fs-006.deepsea.group:")
		     "direct-async-process" t))
  (add-to-list 'tramp-connection-properties
	       (list (concat (regexp-quote "/ssh:") "*" (regexp-quote ".deepsea.group:"))
		     "direct-async-process" t))
  )

(use-package posframe)

;; (when (posframe-workable-p)
;;   (posframe-show " *my-posframe-buffer*"
;;                  :string "This is a test"
;;                  :position (point)))


;; (load (expand-file-name "counsel-tramp.el" user-emacs-directory))
(use-package key-chord
  :config
  (key-chord-mode 1))


(use-package piamh-mode
  :diminish
  :straight '(piamh-mode :local-repo "." :files ("piamh-mode.el"))
  :general
  (my-leader-def
    "b" (general-key-dispatch (lambda () (interactive) (switch-to-buffer "*scratch*"))
	  :timeout 0.25
	  "b" 'piamh/new-empty-buffer)
    "-" 'split-window-below
    "/" 'split-window-right
    "<backspace>" 'delete-window
    "<tab>" 'crux-switch-to-previous-buffer
    "RET" 'newline-and-indent
    "k w" 'delete-window
    "k b" 'kill-buffer
    "k l" 'kill-whole-line
    "K" 'kill-whole-line
    "x a" 'async-shell-command
    "x d f" 'async-shell-command
    )
  ;; asdf
  (general-def "C-a" 'crux-move-beginning-of-line)
  (general-def "<home>" 'crux-move-beginning-of-line)
  (general-def "M-k" 'kill-whole-line)
  (general-def "M-n" (lambda () (interactive) (scroll-up 1)))
  (general-def "M-p" (lambda () (interactive) (scroll-down 1)))
  )

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
