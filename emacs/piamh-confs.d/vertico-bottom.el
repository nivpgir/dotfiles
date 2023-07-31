;;; vertico-bottom.el --- Show the prompt at the bottom of the display. -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "1.4"))
;; Homepage: https://github.com/minad/vertico

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a Vertico extension, which shows the prompt at the
;; bottom of the display.
;;
;; The mode can be enabled globally or via `vertico-multiform-mode'
;; per command or completion category.  Alternatively the display can
;; be toggled temporarily with M-B if `vertico-multiform-mode' is
;; enabled.

;;; Code:

(require 'vertico)
(require 'vertico-multiform)

(defun vertico-bottom--goto (index)
  (let* ((index (car index))
	 (fixed-index (cond
		       ((and (= vertico--index -1) (= index 0)) -1)
		       ((and (= vertico--index -1) (= index -2)) (1- vertico--total))
		       ((>= index vertico--total) -1)
		       ((<= index 0) 0)
		       (t index))))
    (list fixed-index))
  )

(when (and nil "tests")
  (let ((vertico--total 4)
	(vertico--index 1))
    (dolist (case `(
		    (0 . 0)
		    (3 . 3)
		    (4 . -1)
		    (5 . -1)
		    (-1 . 0)
		    (2 . 2)
		    (-3 . 0)
		    (8 . -1)
		    ))
      (let ((res (vertico-bottom--goto (list (car case)))))
	(cl-assert (= (car res) (cdr case)))))
    )
  (let ((vertico--total 4)
	(vertico--index -1))
    (dolist (case `(
		    (0 . -1)
		    ))
      (let ((res (vertico-bottom--goto (list (car case)))))
	(cl-assert (= (car res) (cdr case)) 'show-args "%S :: %S" case res))
      )
    )
  )

;;;###autoload
(define-minor-mode vertico-bottom-mode
  "Show the prompt at the bottom of the display."
  :global t
  (dolist (buf (buffer-list))
    (if vertico-bottom-mode
	(advice-add 'vertico--goto :filter-args 'vertico-bottom--goto)
      (advice-remove 'vertico--goto 'vertico-bottom--goto))
    )
  )

(cl-pushnew 'vertico-bottom-mode vertico-multiform--display-modes)
(vertico-multiform--define-display-toggle bottom)
(define-key vertico-multiform-map (kbd "M-B") #'vertico-multiform-bottom)

(cl-defmethod vertico--display-candidates :after (lines &context (vertico-bottom-mode (eql t)))
  (move-overlay vertico--candidates-ov (point-min) (point-min))
  (let* ((prompt-line (-first-item lines))
	 (candidate-lines (cdr lines))
	 (prompt-at-bottom (-concat candidate-lines (list prompt-line))))
    ;; (setq lines prompt-at-bottom)
    (unless (eq vertico-resize t)
      (setq lines (nconc (make-list (max 0 (- vertico-count (length lines))) "\n") lines)))
    (let ((all-lines-string (apply #'concat lines))
	  (candidate-lines-string (apply #'concat candidate-lines)))
      (add-face-text-property 0 (length all-lines-string) 'default 'append all-lines-string)

      ;; (overlay-put vertico--candidates-ov 'after-string candidate-lines-string)
      ;; (overlay-put vertico--candidates-ov 'before-string nil)

      (overlay-put vertico--candidates-ov 'after-string candidate-lines-string)
      (overlay-put vertico--candidates-ov 'before-string prompt-line)
      )
    (vertico--resize-window (length lines))
    ))

(provide 'vertico-bottom)
;;; vertico-bottom.el ends here
