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



(require 'org-ql)
(require 'org-ql-view)
(require 'ts)

(defun niv/compose (f g)
  `(lambda (x) (,f (,g x))))

(defun niv/prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun niv/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun niv/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun niv/new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer (read-from-minibuffer "New buffer name: "))))
    (switch-to-buffer $buf)
    (setq buffer-offer-save t)
    $buf))

;; go to init.el
(defun niv/find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(defun niv/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun niv/describe-all-keymaps ()
  "Describe all keymaps in currently-defined variables."
  (interactive)
  (with-output-to-temp-buffer "*keymaps*"
    (let (symbs seen)
      (mapatoms (lambda (s)
                  (when (and (boundp s) (keymapp (symbol-value s)))
                    (push (indirect-variable s) symbs))))
      (dolist (keymap symbs)
        (unless (memq keymap seen)
          (princ (format "* %s\n\n" keymap))
          (princ (substitute-command-keys (format "\\{%s}" keymap)))
          (princ (format "\f\n%s\n\n" (make-string (min 80 (window-width)) ?-)))
          (push keymap seen))))
    (with-current-buffer standard-output ;; temp buffer
      (setq help-xref-stack-item (list 'niv/describe-all-keymaps)))))

(defvar my-tasks "~/Sync/organizing/MyTasks.org")

(defun get-last-friday (&optional from-time)
  (let* ((from-time (or from-time (ts-adjust 'day -1 (ts-now)))))
   (adjust-prev-friday (- (mod (- (ts-dow from-time) 5) 7))
    (ts-adjust 'day adjust-prev-friday from-time))))

(defun report-last-week-tasks ()
  (interactive)
  (let ((query))
   `(and
     (planning :from ,(ts-adjust 'day -7 (get-last-friday)))
     (planning :to ,(ts-adjust 'day -1 (get-last-friday))))
    (org-ql-search my-tasks query)))

(provide 'niv-mode)
