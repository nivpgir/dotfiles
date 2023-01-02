(use-package deft
  :general
  (my-leader-def
    "N" 'deft)
  :commands (deft)
  :custom
  (deft-directory "~/Sync/notes/")
  (deft-extensions '("txt" "tex" "org"))
  (deft-recursive t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-new-file-format "%Y-%m-%dT%H%M")
  (deft-file-naming-rules
    '(
      ;; (noslash . "-")
      (nospace . "_")
      (case-fn . downcase))))
