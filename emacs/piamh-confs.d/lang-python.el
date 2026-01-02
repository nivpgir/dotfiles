
(use-package dape
  ;; :config
  ;; (add-to-list 'dape-configs
  ;; 	       '(python-generic-tasks-worker-attach
  ;; 		 host "0.0.0.0"
  ;;                port 5698
  ;; 		 modes (python-ts-mode python-mode)
  ;; 		 :request "attach"))
  ;; (add-to-list 'dape-configs
  ;; 	       '(python-master-attach
  ;; 		 host "0.0.0.0"
  ;;                port 5697
  ;; 		 modes (python-ts-mode python-mode)
  ;; 		 :request "attach"
  ;; 		 )
  ;; 	       )
  ;; (add-to-list 'dape-configs
  ;; 	       '(python-web-attach
  ;; 		 host "0.0.0.0"
  ;;                port 5696
  ;; 		 modes (python-ts-mode python-mode)
  ;; 		 :request "attach")
  ;; 	       )

  ;; (add-to-list 'dape-configs
  ;;              `(python-simple
  ;;                modes (python-ts-mode python-mode)
  ;;                command "python3.10"
  ;;                command-args ("-m" "debugpy.adapter")
  ;;                :type "executable"
  ;;                :request "launch"
  ;;                :cwd dape-cwd-fn
  ;;                :program dape-find-file-buffer-default))
  )

(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix nil)

  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  ;; (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  ;; (setq dape-cwd-function 'projectile-project-root)
  (add-to-list 'dape-configs
	       `(debugpy-with-pytest
		 modes (python-mode python-ts-mode)
		 ensure (lambda (config) t)
		 command "uv"
		 command-args ("run" "python" "-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport)
		 port :autoport
		 :request "launch"
		 :type "python"
		 :cwd dape-cwd
                 :justMyCode nil
                 :console "integratedTerminal"
                 :showReturnValue t
                 :stopOnEntry nil
		 :module "pytest"
		 :args [dape-buffer-default]
		 ))
  )

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :config
  (repeat-mode))
(use-package python-black
  :hook
  ((python-mode python-ts-mode-map) . python-black-on-save-mode-enable-dwim)
  :general
  (:keymaps 'python-mode-map
   "C-c C-f" 'python-black-partial-dwim
   )
  )


(use-package python-pytest)

(use-package poetry)

(require 'lang-python-eglot)
