(setq-default
 user-full-name "Sergey Piratkin"
 user-mail-address "piratkin@tut.by"
 org-directory "~/org/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! kmacro
  :ensure nil
  :custom
  (kmacro-keymap nil))

(use-package! display-line-numbers
  :ensure t
  :hook
  (find-file . display-line-numbers-setup)
  :init
  (defun display-line-numbers-setup ()
    (if (derived-mode-p 'prog-mode)
        (setq
         display-line-numbers-type t
         display-line-numbers t)
      (setq
       display-line-numbers-type nil
       display-line-numbers nil)))
  :config
  (set-face-foreground
   'line-number "brightblack")
  (set-face-foreground
   'line-number-current-line "darkorange"))

(use-package mule
  :ensure nil
  :bind
  (("M-\\" . toggle-input-method))
  :init
  (set-input-method "russian-computer")
  :config
  (toggle-input-method))

(use-package! frame
  :ensure t
  :hook
  (window-setup . toggle-frame-maximized))

(use-package! window
  :ensure nil
  :config
  (map!
   :leader
   (:prefix
    ("w" . "Window")
    "O" #'delete-other-windows
    "o" #'doom/window-enlargen)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! ace-window
  :ensure t
  :config
  (map!
   :leader
   (:prefix
    ("j" . "Jump")
    "w" #'ace-window)
   (:prefix
    ("w" . "Ace Swap Window")
    "w" #'ace-swap-window)))

(use-package! ace-jump-mode
  :ensure t
  :config
  (map!
   :leader
   (:prefix
    ("j" . "Jump")
    "l" #'ace-jump-line-mode
    "j" #'ace-jump-word-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! go-translate
  :ensure t
  :custom
  (gt-translate-list
   '(("en" "ru")
     ("ru" "en")))
  :config
  (map!
   :leader
   (:prefix
    ("l" . "Translate")
    (:prefix ("g" . "Google")
     :desc "overlay"  "o" #'tr-google-overlay
     :desc "buffer"   "b" #'tr-google-buffer
     :desc "insert"   "i" #'tr-google-insert
     :desc "killring" "y" #'tr-google-kill-ring)
    (:prefix ("b" . "Bing")
     :desc "overlay"  "o" #'tr-bing-overlay
     :desc "buffer"   "b" #'tr-bing-buffer
     :desc "insert"   "i" #'tr-bing-insert
     :desc "killring" "c" #'tr-bing-kill-ring)))
  :init
  (defun tr-it (engine render)
    (setq
      gt-default-translator
      (gt-translator
       :taker (gt-taker :langs '(en ru))
       :engines engine
       :render render))
    (gt-do-translate))
  (defun tr-google-overlay ()
    (interactive)
    (tr-it 'gt-google-engine
           'gt-overlay-render))
  (defun tr-google-buffer ()
    (interactive)
    (tr-it 'gt-google-engine
           'gt-buffer-render))
  (defun tr-google-insert ()
    (interactive)
    (tr-it 'gt-google-engine
           'gt-insert-render))
  (defun tr-google-kill-ring ()
    (interactive)
    (tr-it 'gt-google-engine
           'gt-kill-ring-render))
  (defun tr-bing-overlay ()
    (interactive)
    (tr-it 'gt-bing-engine
           'gt-overlay-render))
  (defun tr-bing-buffer ()
    (interactive)
    (tr-it 'gt-bing-engine
           'gt-buffer-render))
  (defun tr-bing-insert ()
    (interactive)
    (tr-it 'gt-bing-engine
           'gt-insert-render))
  (defun tr-bing-kill-ring ()
    (interactive)
    (tr-it 'gt-bing-engine
           'gt-kill-ring-render)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! doom-keybinds
  :ensure t
  :bind
  (("C-SPC" . doom/escape))
  :custom
  (doom-esape-hook))

(use-package! doom-ui
  :ensure t
  :config
  ;; (setq
  ;;  doom-font
  ;;  (font-spec
  ;;   :family "Fira Code"
  ;;   :size 22
  ;;   :weight 'semi-light))
  ;; doom-font
  ;; (font-spec
  ;;  :family "Symbols Nerd Font Mono"
  ;;  :size 14.0)
  ;; doom-font
  ;; (font-spec
  ;;  :family "Fantasque Sans Mono"
  ;;  :size 27.0)
  ;; doom-variable-pitch-font
  ;; (font-spec
  ;;  :family "Fira Sans"
  ;;  :size 23))
  )

(use-package! doom-themes
  :ensure t
  :config
  ;; (load-theme 'doom-pine t)
  ;; (load-theme 'doom-material-dark t)
  ;; (load-theme 'doom-palenight t)
  ;; (load-theme 'doom-horizon t)
  ;; (load-theme 'doom-tokyo-night t)
  ;; (load-theme 'doom-spacegrey t)
  ;; (load-theme 'doom-challenger-deep t)
  ;; (load-theme 'doom-zenburn t)
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-sourcerer t)
  ;; (load-theme 'doom-vibrant t)
  ;; (load-theme 'doom-xcode t)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-theme 'doom-pine)
  :config
  (unless (display-graphic-p)
    (custom-set-faces
     '(dap-ui-marker-face
       ((t (:background "#050510"))))
     '(dap-ui-pending-breakpoint-face
       ((t (:background "#222222"))))
     '(dap-ui-verified-breakpoint-face
       ((t (:background "#100505"))))))
  (set-face-background 'mode-line "gray20")
  (set-face-foreground 'mode-line "white")
  (set-face-background 'vertical-border "gray10")
  (set-face-background 'mode-line-inactive "gray10")
  (set-face-foreground 'mode-line-inactive "brightblack"))

(use-package! doom-modeline
  :ensure t
  :config
  (unless (display-graphic-p)
  (when (member doom-theme '(doom-one))
    (custom-set-faces!
      '(mode-line :underline t))))
  :custom
  (doom-modeline-modal t)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-major-mode-color-icon nil)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-check-simple-format t)
  (doom-modeline-modal-modern-icon t)
  (doom-modeline-modal-icon nil)
  (doom-modeline-check-icon t)
  (doom-modeline-vcs-icon t)
  (doom-modeline-icon t)
  (doom-modeline-lsp t)
  (inhibit-compacting-font-caches t)
  (find-file-visit-truename t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  (unless (all-the-icons-fonts-installed-p)
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :requires all-the-icons
  :after dired
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! evil-escape
  :after evil
  :ensure t
  :config
  (setq-default
   evil-escape-delay nil
   evil-escape-key-sequence nil))

(use-package! evil-commands
  :ensure t
  :after evil
  :bind
  (:map evil-normal-state-map
   ;; ("K" . nil) ("Л" . nil)
   ("з" . evil-paste-after)
   ("н" . evil-yank)
   ("Ф" . evil-append-line)
   ("Ш" . evil-insert-line)
   ("с" . evil-change)
   ("к" . evil-replace)
   ("в" . evil-delete)
   ("ч" . evil-delete-char)
   ("г" . evil-undo)
   ("ш" . evil-insert)
   ("щ" . evil-open-below)
   ("О" . evil-join)
   ;; :map evil-insert-state-map
   ;; ("K" . nil) ("Л" . nil)
   ;; :map evil-visual-state-map
   ;; ("K" . nil) ("Л" . nil)
   :map evil-motion-state-map
   ;; ("K" . nil) ("Л" . nil)
   ("C-j" . evil-scroll-down)
   ("C-k" . evil-scroll-up)
   ("C-h" . evil-window-top)
   ("C-m" . evil-window-middle)
   ("C-l" . evil-window-bottom)
   ("C-о" . evil-scroll-down)
   ("C-л" . evil-scroll-up)
   ("C-р" . evil-window-top)
   ("C-ь" . evil-window-middle)
   ("C-д" . evil-window-bottom)
   ;; ("H" . evil-window-top)
   ;; ("L" . evil-window-bottom)
   ;; ("Р" . evil-window-top)
   ;; ("Д" . evil-window-bottom)
   ("ц" . evil-forward-word-begin)
   ("и" . evil-backward-word-begin)
   ("у" . evil-forward-word-end)
   ("Ж" . evil-ex)
   ("л" . evil-previous-line)
   ("о" . evil-next-line)
   ("р" . evil-backward-char)
   ("д" . evil-forward-char)
   ("т" . evil-ex-search-next)
   ("Т" . evil-ex-search-previous)
   ("н" . evil-yank))
  :config
  (map!
   :leader
   (:prefix
    ("w" . "Window")
    "]" #'evil-window-increase-height
    "[" #'evil-window-decrease-height
    ">" #'evil-window-increase-width
    "<" #'evil-window-decrease-width
    "k" #'evil-window-up
    "j" #'evil-window-down
    "h" #'evil-window-left
    "l" #'evil-window-right)))

(use-package! evil-collection
  :after evil
  :ensure t
  ;; :config
  ;; (setq evil-collection-setup-minibuffer t)
  ;; (evil-collection-init)
  )

(use-package! evil-states
  :ensure t
  :after evil
  :bind
  (:map evil-motion-state-map
   ("м" . evil-visual-char)
   ("М" . evil-visual-line)))

(use-package! evil-vars
  :ensure t
  :config
  (setq evil-kill-on-visual-paste nil))

(use-package! evil
  :requires repeat
  :ensure t
  :config
  (map!
   :leader
   (:prefix
    ("w" . "Window")
    "K" #'+evil/window-move-up
    "J" #'+evil/window-move-down
    "H" #'+evil/window-move-left
    "L" #'+evil/window-move-right))
  :config
  (evil-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! general
  :ensure t
  :config
  (map!
   :leader
   (:prefix
    ("w" . "Window")
    "0" nil "1" nil "2" nil "3" nil "4" nil
    "5" nil "6" nil "7" nil "8" nil "9" nil
    "+" nil "-" nil "b" nil "f" nil "p" nil
    "w" nil "W" nil
    "C-q" nil "C-=" nil "C-_" nil "C-w" nil
    "C-c" nil "C-f" nil "C-j" nil "C-k" nil
    "C-l" nil "C-n" nil "C-o" nil "C-p" nil
    "C-b" nil "C-h" nil "C-r" nil "C-s" nil
    "C-t" nil "C-u" nil "C-v" nil "C-x" nil
    "C-S-h" nil "C-S-j" nil "C-S-k" nil
    "C-S-l" nil "C-S-r" nil "C-S-s" nil
    "C-S-w" nil
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! repeat
  :ensure t
  :hook
  (dap-mode . repeat-mode)
  (evil-mode . repeat-mode)
  :init
  (defvar dap-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "n" 'dap-next)
      (define-key map "s" 'dap-step-in)
      (define-key map "S" 'dap-step-out)
      (define-key map "c" 'dap-continue)
      (define-key map "b" 'dap-breakpoint-toggle)
      (define-key map "t" 'dap-switch-thread)
      (define-key map "f" 'dap-switch-stack-frame)
      (define-key map "p" 'dap-switch-session)
      (define-key map "q" 'dap-disconnect)
      map)
    "Keymap for repeating DAP commands.")
  :custom
  (repeat-exit-key "<escape>")
  :config
  (defmacro set-repeat-map (funcs)
    `(dolist (func ',funcs)
       (put func 'repeat-map 'dap-repeat-map)))
  (set-repeat-map
   (dap-next
    dap-step-in
    dap-step-out
    dap-continue
    dap-breakpoint-toggle
    dap-switch-thread
    dap-switch-stack-frame
    dap-switch-session)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company-box
  :after company
  :hook
  (company-mode . company-box-mode)
  :ensure t)

(use-package! company-lsp
  :after company
  ;; :hook
  ;; (company-mode . company-lsp-mode)
  :config
  (setq-default
   company-lsp-async t
   company-lsp-enable-recompletion t
   company-lsp-cache-candidates nil)
  (push 'company-lsp company-backends)
  :ensure t)

(use-package! company
  :config
  (global-company-mode 1)
  :ensure t)

;; (use-package! company-c-headers
;;   :ensure t
;;   :after company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! lsp-ui
  :after lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :ensure t)

(use-package! lsp-mode
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (python-mode . lsp)
  :commands lsp)

;; (use-package! lsp-lens
;;   :delight
;;   :config
;;   (lsp-lens-mode " lns"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! dap-ui
  :after dap-mode
  :hook
  (dap-mode . dap-ui-mode)
  :custom
  (dap-ui-controls-mode nil)
  (dap-ui-many-windows-mode nil)
  (dap-ui-variable-length 255)
  :ensure t)

(use-package! dap-mode
  :custom
  (doom-debug-mode t)
  (gdb-many-windows nil)
  (dap-auto-configure-mode nil)
  (dap-auto-show-output nil)
  (dap-print-io nil)
  (tooltip-mode nil)
  (dap-tooltip-mode nil)
  (dap-ui-buffer-configurations
   '((nil . nil)))
  (dap-auto-configure-features
   '(locals sessions repl))
  :config
  (dap-mode 1)
  (map! :leader
   (:prefix ("d" . "Debug")
    "d" #'dap-debug
    "r" #'dap-debug-last
    "R" #'dap-debug-recent
    "n" #'dap-next
    "s" #'dap-step-in
    "S" #'dap-step-out
    "c" #'dap-continue
    "b" #'dap-breakpoint-toggle
    "B" #'dap-breakpoint-condition
    "D" #'dap-breakpoint-delete-all
    "q" #'dap-disconnect
    "t" #'dap-switch-thread
    "f" #'dap-switch-stack-frame
    "p" #'dap-switch-session
    ;; "k" #'dap-break
    ;; "r" #'dap-restart
    "Q" #'dap-terminated-debug
    (:prefix ("u" . "Ui")
     "U" #'dap-ui-repl
     "l" #'dap-ui-locals
     ;; "b" #'dap-breakpoint-list
     ;; "r" #'dap-ui-restart
     ;; "w" #'dap-ui-watch
     ;; "s" #'dap-ui-session
     ;; "i" #'dap-ui-inspect
     "h" #'dap-hydra)))
  :ensure t)

;; DAP_ENDPOINT = host:port
;; DAP_CWD_PATH = $PWD
;; DAP_APP_PATH = a.exe
;; DAP_DBG_PATH = /usr/bin/gdb
;; DAP_APP_ARGS = --debuglevel=10
;; DAP_DBG_ARGS = --silent
;; DAP_DBG_CMDS = break main
;; DAP_ENV_VARS =

(use-package! dap-python
  :disabled
  :after dap-mode
  :if (executable-find "python3")
  ;; :hook
  ;; (python-mode . dap-python-mode)
  :config
  (dap-unregister-debug-template
   "Python::Run Configuration")

  ;; (dap-register-debug-template
  ;;  "python::launch"
  ;;  '(:type "python"
  ;;    :args ""
  ;;    :cwd "${workspaceFolder}"
  ;;    :program nil
  ;;    :module nil
  ;;    :justMyCode t
  ;;    :request "launch"))

  ;; (dap-register-debug-template
  ;;  "python::attach"
  ;;  '(:type "python"
  ;;    :processId "${command:pickProcess}"
  ;;    :justMyCode t
  ;;    :request "attach"))

  ;; (dap-python-setup t)
  :ensure t)

(use-package! dap-cpptools
  :disabled
  :after dap-mode
  :if (executable-find "gdb")
  ;; :hook
  ;; (c-mode-common . dap-cpptools-mode)
  :config
  (dap-unregister-debug-template
   "CppTools::Run Configuration")
  ;;
  ;; (dap-register-debug-template
  ;;  "dap::cpptools::launch::simple"
  ;;  `(:type "cppdbg"
  ;;    :request "launch"
  ;;    :program
  ;;    ,(getenv "DAP_APP")
  ;;    :cwd
  ;;    ,(getenv "DAP_CWD")
  ;;    :MIMode "gdb"
  ;;    :miDebuggerPath
  ;;    ,(getenv "DAP_DBG")
  ;;    :stopAtEntry t))
  ;; ;;
  ;; (dap-register-debug-template
  ;;  "dap::cpptools::launch::default"
  ;;  `(:type "cppdbg"
  ;;    :request "launch"
  ;;    :program
  ;;    ,(getenv "DAP_APP_PATH")
  ;;    :cwd
  ;;    ,(getenv "DAP_CWD_PATH")
  ;;    :MIMode "gdb"
  ;;    :targetArchitecture "x86_64"
  ;;    :miDebuggerPath
  ;;    ,(getenv "DAP_DBG_PATH")
  ;;    :miDebuggerArgs
  ;;    ,(getenv "DAP_GDB_ARGS")
  ;;    :externalConsole nil
  ;;    :justMyCode t)
  ;;    ;; :useExtendedRemote t
  ;;    ;; :miDebuggerServerAddress
  ;;    ;; "localhost:1234"
  ;;    ;; :postRemoteConnectCommands
  ;;    :args ,(vconcat (split-string
  ;;       (getenv "D AP_APP_ARGS")))
  ;;    :stopAtEntry t)
  ;;
  ;; (dap-register-debug-template
  ;;  "cppgdb::python::pid::attach"
  ;;  '(:name "cppgdb::python::pid::attach"
  ;;    :type "cppgdb"
  ;;    :request "attach"
  ;;    :program "/usr/bin/python3"
  ;;    :additionalSOLibSearchPath
  ;;    (concat
  ;;     "/usr/local/samba/lib/python3.7/site-packages/samba;"
  ;;     "/usr/local/samba/lib;"
  ;;     "/usr/local/samba/lib/ldb")
  ;;    :processId "${command:pickProcess}"
  ;;    :MIMode "gdb"
  ;;    :justMyCode t
  ;;    :printCalls t))

  ;; (dap-cpptools-setup t)
  :ensure t)

(use-package! dap-gdb-lldb
  :after dap-mode
  :if (or (executable-find "gdb")
          (executable-find "lldb"))
  :init
  (dap-register-debug-template
   "dap::gdb_lldb::launch::simple"
   `(:type "gdb"
     :request "launch"
     :target ,(getenv "DAP_APP_PATH")
     :cwd ,(getenv "DAP_CWD_PATH")
     :args ,(vconcat (split-string
       (getenv "DAP_APP_ARGS")))
     :autorun ,(vconcat
       (mapcar 'string-trim
         (split-string (getenv
           "DAP_DBG_CMDS") ":" t)))
     :debugger_args
         ,(vconcat (mapcar 'string-trim
           (split-string (getenv
         "DAP_DBG_ARGS"))))
     :env ,(vconcat (split-string
       (getenv "DAP_ENV_VARS")))))
  (dap-register-debug-template
   "dap::gdbserver::attach::simple"
   `(:type "gdbserver"
     :request "attach"
     :target ,(getenv "DAP_ENDPOINT")
     :cwd ,(getenv "DAP_CWD_PATH")
     :args ,(vconcat (split-string
       (getenv "DAP_APP_ARGS")))
     :autorun ,(vconcat
       (mapcar 'string-trim
         (split-string (getenv
           "DAP_DBG_CMDS") ":" t)))
     :debugger_args
     ,(vconcat (mapcar 'string-trim
       (split-string (getenv
         "DAP_DBG_ARGS"))))
     :env ,(vconcat (split-string
       (getenv "DAP_ENV_VARS")))))
  :config
  (dap-gdb-lldb-setup)
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package! evil-macro
;;   :ensure t)

;; (setenv "DAP_ENV_VARS" "PATH=/app/inno-samba/sbin:/app/inno-samba/bin:$PATH")

     ;; :request "attach"
     ;; :gdbpath "gdb"
;; (use-package! dap-gdb
;;   :disabled
;;   :after dap-mode
;;   :if (executable-find "gdb")
;;   :config
;;   ;; (dap-register-debug-template
;;   ;;  "dap::gdbserver::attach::simple"
;;   ;;  `(:type "gdbserver"
;;   ;;    :request "attach"
;;   ;;    :executable
;;   ;;    ,(getenv "DAP_APP_PATH")
;;   ;;    :debugger_args
;;   ;;    ,(vconcat (mapcar 'string-trim
;;   ;;      (split-string (getenv
;;   ;;        "DAP_DBG_ARGS"))))
;;   ;;    :env ,(vconcat (split-string
;;   ;;       (getenv "DAP_APP_ARGS")))
;;   ;;    :gdbpath "gdb"
;;   ;;    :cwd ,(getenv "DAP_CWD_PATH")
;;   ;;    :target
;;   ;;    ,(getenv "DAP_ENDPOINT")
;;   ;;    :autorun
;;   ;;    ,(vconcat (mapcar 'string-trim
;;   ;;      (split-string (getenv
;;   ;;        "DAP_DBG_CMDS") ":" t)))
;;   ;;    :valuesFormatting "prettyPrinters"
;;   ;;    :showDevDebugOutput nil
;;   ;;    :printCalls nil
;;   ;;    :remote t))
;;   ;; (dap-gdb-setup t)
;;   :ensure t)

;; (use-package! dap-lldb
;;   :disabled
;;   :after dap-mode
;;   :if (executable-find "lldb")
;;   :config
;;   (dap-lldb-setup)
;;   :ensure t)



  ;; (dap-register-debug-template
  ;;  "dap::gdbserver::attach::simple"
  ;;  `(:type "gdb"
  ;;    :request "lounch"
  ;;    :target "/app/inno-samga/sbin/samba"
  ;;    :cwd "/home/user/Work"
  ;;    :executable "/app/inno-samga/sbin/samba"
  ;;    :debugger_args []
  ;;    :env []
  ;;    :gdbpath "/usr/bin/gdb"
  ;;    :autorun ["break main"]
  ;;    :showDevDebugOutput nil
  ;;    :valuesFormatting "prettyPrinters"
  ;;    :remote t
  ;;    :printCalls nil
  ;;    ))
