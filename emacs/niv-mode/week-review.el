

(use-package org-ql
  :straight t)

(require 'org-ql)
(require 'org-ql-view)
(require 'ts)

;; #+BEGIN: org-ql :query "todo: priority:A,B" :columns (todo (priority "P") ((property "agenda-group") "Group") deadline heading) :sort (deadline priority) :take 7 :ts-format "%Y-%m-%d %H:%M"
;; | Todo | P | Group | Deadline         | Heading                               |
;; |------+---+-------+------------------+---------------------------------------|
;; | TODO | A |       | 2017-07-07 00:00 | Take over the world                   |
;; | TODO | B |       | 2017-07-10 00:00 | Renew membership in supervillain club |
;; | TODO | A | plans | 2017-07-15 00:00 | Take over the universe                |
;; | TODO | B |       | 2017-07-21 00:00 | Internet                              |
;; | TODO | A | bills | 2017-08-01 00:00 | Spaceship lease                       |
;; | TODO | A |       |                  | Skype with president of Antarctica    |
;; | TODO | B |       |                  | Take over Mars                        |
;; #+END:


(defvar my-tasks "~/Sync/organizing/MyTasks.org")

(defvar test-query
  `(and (planning :from ,(ts-adjust 'day -7 (get-last-friday)))
	(planning  :to ,(get-last-friday))))

(defun report-last-week-tasks ()
  (interactive)
  (let ((query `(and
		 (planning :from ,(ts-adjust 'day -7 (get-last-friday)))
		 (planning :to ,(ts-adjust 'day -1 (get-last-friday))))))
    (org-ql-search my-tasks query)))

(let ((query `(and
	       (ts :from ,(ts-adjust 'day -7 (get-last-friday)))
	       (ts :to ,(ts-adjust 'day -1 (get-last-friday))))))
  (org-ql-select my-tasks query))




(defun get-last-friday (&optional from-time)
  (let* ((from-time (or from-time (ts-adjust 'day -1 (ts-now))))
	 (adjust-prev-friday (- (mod (- (ts-dow from-time) 5) 7))))
    (ts-adjust 'day adjust-prev-friday from-time)))

(ts-format (get-last-friday (get-last-friday)))

(defun get-previous-work-week ()
  (let* ((prev-fri (get-last-friday))
	 (prev-prev-fri (get-last-friday (ts-adjust 'day -1 prev-fri))))
    (cons (ts-format prev-fri) (ts-format (get-last-friday prev-prev-fri)))))

(get-previous-work-week)


(defun first-day-of-week ()
  (let* ((now (ts-now))
	 (adjust-first-day (- (+ 7 (ts-dow now)))))
    (thread-last now
      (ts-adjust 'day adjust-first-day)
      (ts-apply :hour 0 :minute 0 :second 0))))

(defun last-day-of-week ()
  (let* ((now (ts-now))
	 (adjust-last-day (- (- 7 (- 6 (ts-dow now))))))
    (thread-last now
      (ts-adjust 'day adjust-last-day)
      (ts-apply :hour 23 :minute 59 :second 59))))

(cons (ts-format (first-day-of-week)) (ts-format (last-day-of-week)))

(defun last-week-range ()
  "Return timestamps (BEG . END) spanning the previous calendar week."
  (let* ((now (ts-now))
	 (adjust-beg-day (- (+ 7 (ts-dow now))))
	 (adjust-end-day (- (- 7 (- 6 (ts-dow now)))))
	 (beg (thread-last now
		(ts-adjust 'day adjust-beg-day)
		(ts-apply :hour 0 :minute 0 :second 0)))
	 (end (thread-last now
		(ts-adjust 'day adjust-end-day)
		(ts-apply :hour 23 :minute 59 :second 59))))
    (cons beg end)))

(last-week-range)

(-let* ((ts-default-format "%a, %Y-%m-%d %H:%M:%S %z")
	(check-ts (ts-adjust 'day -3 (ts-now)))
	((beg . end) (last-week-range)))
  (list :last-week-beg (ts-format beg)
        :check-ts      (ts-format check-ts)
        :last-week-end (ts-format end)
        :in-range-p    (ts-in beg end check-ts)))
