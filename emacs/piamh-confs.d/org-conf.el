
(use-package doct
  :straight t
  :commands (doct))


(use-package org
  :straight org-contrib
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (dot . t)))
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)	; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  ;; diff hooks for org mode
  ;; Check for org mode and existence of buffer
  (defun f-ediff-org-showhide(buf command &rest cmdargs)
    "If buffer exists and is orgmode then execute command"
    (if buf
	(if (eq (buffer-local-value 'major-mode (get-buffer buf)) 'org-mode)
            (save-excursion (set-buffer buf) (apply command cmdargs)))))

  (defun f-ediff-org-unfold-tree-element ()
    "Unfold tree at diff location"
    (f-ediff-org-showhide ediff-buffer-A 'org-reveal)
    (f-ediff-org-showhide ediff-buffer-B 'org-reveal)
    (f-ediff-org-showhide ediff-buffer-C 'org-reveal))

  (defun f-ediff-org-fold-tree ()
    "Fold tree back to top level"
    (f-ediff-org-showhide ediff-buffer-A 'hide-sublevels 1)
    (f-ediff-org-showhide ediff-buffer-B 'hide-sublevels 1)
    (f-ediff-org-showhide ediff-buffer-C 'hide-sublevels 1))

  (defun org-time-stamp-no-prompt ()
    (insert (format-time-string)
	    (org-time-stamp-format 'long)
	    (org-current-effective-time)))

  (defun org-time-stamp-inactive-no-prompt ()
    (insert (format-time-string)
	    (org-time-stamp-format 'long 'inactive)
	    (org-current-effective-time)))

  (defun org-habit-count-completed ()
    (count-matches
     (char-to-string org-habit-completed-glyph)
     (line-beginning-position) (line-end-position)))

  (defun org-habit-line-p (point)
    (get-text-property point 'org-habit-p))

  (defun insert-on-line-end (string)
    (save-excursion
      (end-of-line)
      (insert string)))

  (defmacro for-each-line-of-buffer (body)
    `(save-excursion
       (point-min)
       (while (not (eobp))
	 ,body
	 (forward-line 1))))

  (defun org-habit-streak-count ()
    (for-each-line-of-buffer
     (when (org-habit-line-p (point))
       (insert-on-line-end
	(number-to-string (org-habit-count-completed))))))
  :hook
  (org-after-todo-statistics . org-summary-todo)
  (ediff-select . f-ediff-org-unfold-tree-element)
  (ediff-unselect . f-ediff-org-fold-tree)
  :custom
  ;; for more complex stuff look at `org-depend.el'
  (org-enforce-todo-dependencies t "block setting to DONE until previous siblings and children are DONE")
  (org-enforce-todo-checkbox-dependencies t "same as above but for checkboxes")
  (org-cycle-separator-lines 0)
  (org-extend-today-until 3)
  (org-use-effective-time t)
  (org-log-reschedule 'time)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-targets '((nil :maxlevel . 3)
			(org-agenda-files :maxlevel . 9)))

  (org-capture-templates
   (doct '(("Todo"
	    :keys "t"
	    :file "~/Sync/organizing/pending-tasks.org"
	    :todo-state "TODO"
	    :template ("* %{todo-state} %^{Description}"
		       ":PROPERTIES:"
		       ":CREATED: %U"
		       ":END:"
		       "%a"
		       "%i%?"))
	   ("Event" :keys "e"
	    :template ("* %^{Who?/What?}"
		       ":PROPERTIES:"
		       ":CREATED: %U"
		       ":END:"
		       "%^{When}T"
		       "%a"
		       "%i%?")
	    :children (("Personal"
			:keys "p"
			:file "~/Sync/organizing/personal.org"
			:olp ("Events"))
		       ("Work"
			:keys "w"
			:file "~/Sync/organizing/cady/tasks.org"
			:olp ("Events")))))))
  :general
  (my-leader-def
    "c" 'org-capture
    "os" (general-key-dispatch 'org-sort
	   :timeout 0.25
	   "l" 'org-store-link)
    ;; "osl" 'org-store-link
    "odl" 'org-insert-last-stored-link
    )
  ("M-m s" 'org-todo)
  ("M-m b" 'org-metaleft)
  ("M-m f" 'org-metaright)
  ("C-M-<return>" 'org-insert-subheading)
  ("M-m t" 'org-insert-structure-template)
  ("C-M-S-<return>" 'org-insert-todo-subheading)
  )

(use-package org-agenda
  :straight org
  :after org
  :hook
  (org-agenda-finalize . org-habit-streak-count)
  :general
  (my-leader-def
    "a" 'org-agenda)
  :custom
  (org-agenda-files (list "~/Sync/organizing"))
  (org-agenda-start-on-weekday 6)
  ;; (org-agenda-start-day "-1d")
  (org-agenda-custom-commands nil)
  (org-agenda-sorting-strategy '((agenda habit-down time-up category-keep tag-up priority-down))
			       (todo priority-down category-keep)
			       (tags priority-down category-keep)
			       (search category-keep))
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                              ;; Indent todo items by level to show nesting
                              (todo . " %i %-12:c%l")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  (org-agenda-log-mode-items '(closed clock state))
  :config
  (add-to-list 'org-agenda-custom-commands
	       '("d" . "Todays views"))
  (add-to-list 'org-agenda-custom-commands
	       '("dd" agenda "Todays agenda" ((org-agenda-span 'day))))
  (add-to-list 'org-agenda-custom-commands
	       '("dc" agenda "Cadys agenda"
		 ((org-agenda-files '("~/Sync/organizing/cady/tasks.org"))
		  (org-agenda-start-day ".")
		  (org-agenda-span 'day))))
  (add-to-list 'org-agenda-custom-commands
	       '("dm" agenda "My agenda"
		 ((org-agenda-files '("~/Sync/organizing/personal.org"))
		  (org-agenda-start-day ".")
		  (org-agenda-span 'day))))
  (add-to-list 'org-agenda-custom-commands
               '("w" . "Weekly views"))
  (add-to-list 'org-agenda-custom-commands
               '("ww" "Weekly summary"
		 agenda ""
		 ((org-agenda-start-day "-1w")
                  (org-agenda-span 7)
                  (org-agenda-start-with-log-mode '(closed))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE ")))))
  (add-to-list 'org-agenda-custom-commands
               '("wc" "Cady weekly view"
		 agenda ""
		 ((org-agenda-files '("~/Sync/organizing/cady/tasks.org"))
		  (org-agenda-start-day "-1w")
                  (org-agenda-span 7)
                  (org-agenda-start-with-log-mode '(closed))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE ")))))
  (add-to-list 'org-agenda-custom-commands
               '("wm" "My weekly view"
		 agenda ""
		 ((org-agenda-files '("~/Sync/organizing/personal.org"))
		  (org-agenda-start-day "-1w")
                  (org-agenda-span 7)
		  (org-agenda-start-with-log-mode '(closed))
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*\\* DONE ")))))
  )

(use-package org-habit
  :straight org-contrib
  :config
  (require 'org-habit)
  :custom
  (org-habit-preceding-days 60))

(use-package org-expiry
  :straight org-contrib
  :after org
  :custom
  (org-expiry-created-property-name "CREATED") ; Name of property when an item is created
  (org-expiry-inactive-timestamps   t)
  :config
  (advice-add 'org-insert-heading :after 'org-expiry-insert-created)
  )

