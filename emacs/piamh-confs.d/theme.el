;;; Theme
(use-package vscode-dark-plus-theme
  :straight t
  :custom
  (vscode-dark-plus-scale-org-faces nil)
  (vscode-dark-plus-invert-hl-todo nil)
  :config
  (load-theme 'vscode-dark-plus t))


;;; reminders on how to use fonts:
;; (seq-map (lambda (font)
;; 	   (let ((info (font-info font)))
;; 	     (if info (aref info 1)
;; 	       nil)))
;; 	 (font-family-list))
;; (font-family-list)
;; (set-face-attribute 'default nil
;; 		    :family "Ubuntu Mono"
;; 		    :height 120)
;; (set-face-attribute 'default nil
;; 		    :family "Hack"
;; 		    :height

;;;actual font settings
(setq my-prefered-font
      (cond ((eq system-type 'windows-nt) "hack")
            ((eq system-type 'gnu/linux) "Noto Sans Mono")
            (t nil)))

(when my-prefered-font
  (add-to-list 'default-frame-alist
             `(font . ,my-prefered-font))

  )

(add-to-list 'face-ignored-fonts "Noto Rashi Hebrew")
(add-to-list 'face-ignored-fonts "Noto Serif Hebrew")

(font-family-list)
(set-face-attribute 'default nil :height 110)
