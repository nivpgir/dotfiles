(use-package deft
  :general
  (:keymaps 'deft-mode-map
	   "C-c C-d" 'deft-delete-file)
  (my-leader-def
    "N" 'deft)
  :commands (deft)
  :custom
  (deft-directory "~/Sync/notes/")
  (deft-extensions '("md" "txt" "tex" "org"))
  (deft-recursive t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-new-file-format "%Y-%m-%dT%H%M%S")
  (deft-file-naming-rules
    '(
      ;; (noslash . "-")
      (nospace . "_")
      (case-fn . downcase))))
