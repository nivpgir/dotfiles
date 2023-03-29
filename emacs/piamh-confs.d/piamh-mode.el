;; Main use is to have my key bindings have the highest priority
;; https://github.com/kaushalmodi/.emacs.d/blob/master/elisp/modi-mode.el

(defvar piamh-mode-map (make-sparse-keymap)
  "Keymap for `piamh-mode'.")

;;;###autoload
(define-minor-mode piamh-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-piamh-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " piamh-mode"
  :keymap piamh-mode-map)

;;;###autoload
(define-globalized-minor-mode global-piamh-mode piamh-mode piamh-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((piamh-mode . ,piamh-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun piamh/turn-off-piamh-mode ()
  "Turn off piamh-mode."
  (piamh-mode -1))
(add-hook 'minibuffer-setup-hook #'piamh/turn-off-piamh-mode)


(provide 'piamh-mode)
