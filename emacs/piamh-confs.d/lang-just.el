(use-package just-mode
  :straight t)

(use-package justl
  :straight (justl
	     :type git
	     :host github
	     :repo "psibi/justl\.el")
  :custom
  (justl-executable "c:/Users/Niv/.local/scoop/shims/just.exe"))
