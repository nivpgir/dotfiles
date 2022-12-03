;; Main use is to have my key bindings have the highest priority
;; https://github.com/kaushalmodi/.emacs.d/blob/master/elisp/modi-mode.el

(defvar niv-mode-map (make-sparse-keymap)
  "Keymap for `niv-mode'.")

;;;###autoload
(define-minor-mode niv-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-niv-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " niv-mode"
  :keymap niv-mode-map)

;;;###autoload
(define-globalized-minor-mode global-niv-mode niv-mode niv-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((niv-mode . ,niv-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun niv/turn-off-niv-mode ()
  "Turn off niv-mode."
  (niv-mode -1))
(add-hook 'minibuffer-setup-hook #'niv/turn-off-niv-mode)


(provide 'niv-mode)
