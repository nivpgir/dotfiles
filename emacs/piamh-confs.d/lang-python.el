
(use-package dape
  :config
  (add-to-list 'dape-configs
               `(python-simple
                 modes (python-ts-mode python-mode)
                 command "python3.10"
                 command-args ("-m" "debugpy.adapter")
                 :type "executable"
                 :request "launch"
                 :cwd dape-cwd-fn
                 :program dape-find-file-buffer-default))
  )

(use-package python-black
  :hook
  ((python-mode python-ts-mode-map) . python-black-on-save-mode-enable-dwim)
  :general
  (:keymaps 'python-mode-map
   "C-c C-f" 'python-black-partial-dwim
   )
  )


(use-package python-pytest)

(use-package python-black)

(use-package poetry)

(require 'lang-python-eglot)
