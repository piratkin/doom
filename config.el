(setq-default
 user-full-name "Sergey Piratkin"
 user-mail-address "piratkin@tut.by"
 enable-local-variables nil
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
  (window-setup . toggle-frame-maximized)
  :custom
  (window-divider-default-places t))

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
  :config
  (map!
   :leader
   (:prefix
    ("l" . "Translate")
    (:prefix ("g" . "Google")
     :desc "overlay"
     "o" #'tr-google-overlay
     :desc "buffer"
     "b" #'tr-google-buffer
     :desc "insert"
     "i" #'tr-google-insert
     :desc "killring"
     "y" #'tr-google-kill-ring)
    (:prefix ("b" . "Bing")
     :desc "overlay"
     "o" #'tr-bing-overlay
     :desc "buffer"
     "b" #'tr-bing-buffer
     :desc "insert"
     "i" #'tr-bing-insert
     :desc "killring"
     "c" #'tr-bing-kill-ring)))
  :init
  (setq-default
   gt-translate-list
   '(("en" "ru")
     ("ru" "en")))
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

;; (use-package! doom-keybinds
;;   :ensure t
;;   :bind
;;   (("C-SPC" . doom/escape))
;;   :custom
;;   (doom-esape-hook))

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
  (set-face-foreground 'mode-line "darkgrey")
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
  :config
  (setq-default
   evil-escape-delay nil
   evil-escape-key-sequence nil)
  :ensure t)

(use-package! evil-commands
  :ensure t
  :after evil
  :init
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
    "C-S-w" nil))
  :bind
  ("C-j" . evil-scroll-down)
  ("C-k" . evil-scroll-up)
  ("C-h" . evil-window-top)
  ("C-l" . evil-window-bottom)
  ("C-о" . evil-scroll-down)
  ("C-л" . evil-scroll-up)
  ("C-р" . evil-window-top)
  ("C-д" . evil-window-bottom)
  (:map evil-normal-state-map
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
   ;; ("RET" . newline-and-indent)
   :map evil-motion-state-map
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
    "k" #'evil-window-up
    "j" #'evil-window-down
    "h" #'evil-window-left
    "l" #'evil-window-right)))

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

(use-package! repeat
  :ensure t
  :hook
  (dap-mode . repeat-mode)
  (evil-mode . repeat-mode)
  :init
  (defvar win-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "]" 'evil-window-increase-height)
      (define-key map "[" 'evil-window-decrease-height)
      (define-key map "}" 'evil-window-increase-width)
      (define-key map "{" 'evil-window-decrease-width)
       map)
    "Keymap for repeating Window commands.")
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
      (define-key map "q" 'dap-disconnect) map)
    "Keymap for repeating DAP commands.")
  :custom
  (repeat-exit-key "<escape>")
  :config
  (map!
   :leader
   (:prefix
    ("w" . "Window")
    "]" #'evil-window-increase-height
    "[" #'evil-window-decrease-height
    "}" #'evil-window-increase-width
    "{" #'evil-window-decrease-width))
  (defmacro set-win-repeat-map (funcs)
    `(dolist (func ',funcs)
       (put func 'repeat-map 'win-repeat-map)))
  (defmacro set-dap-repeat-map (funcs)
    `(dolist (func ',funcs)
       (put func 'repeat-map 'dap-repeat-map)))
  (set-win-repeat-map
   (evil-window-increase-height
    evil-window-decrease-height
    evil-window-increase-width
    evil-window-decrease-width
    evil-window-up
    evil-window-down
    evil-window-left
    evil-window-right))
  (set-dap-repeat-map
   (dap-next
    dap-step-in
    dap-step-out dap-continue
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
  :init
  (setq-default
   dap-ui-buffer-configurations
   `((,dap-ui--locals-buffer .
       ((side . right) (slot . 1)
      (window-width . 0.25)))
     (,dap-ui--expressions-buffer .
       ((side . right) (slot . 2)
      (window-width . 0.25)))
     (,dap-ui--sessions-buffer .
       ((side . right) (slot . 3)
      (window-width . 0.25)))
     (,dap-ui--breakpoints-buffer .
       ((side . left) (slot . 2)
      (window-width . 0.25)))
     (,dap-ui--debug-window-buffer .
       ((side . bottom) (slot . 3)
      (window-height . 0.15)))
     (,dap-ui--repl-buffer .
       ((side . bottom) (slot . 1)
      (window-height . 0.15)))))
  :config
  ;; (evil-define-key 'insert dap-ui-repl-mode-map
  ;;   (kbd "RET") #'comint-send-input)
  (setq-default
   dap-ui-controls-mode t
   dap-ui-many-windows-mode t
   dap-ui-variable-length 255)
  :ensure t)

(use-package! dap-mode
  :config
  (setq-default
   doom-debug-mode t
   gdb-show-main t
   gdb-many-windows t
   dap-auto-show-output t
   dap-print-io t
   tooltip-mode nil
   dap-tooltip-mode nil
   ;; dap-auto-configure-mode nil
   dap-auto-configure-features
   '(locals sessions))
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
    "Q" #'dap-terminated-debug
    (:prefix ("u" . "Ui")
     "r" #'dap-ui-repl-company
     "p" #'dap-ui-repl-company-prefix
     "p" #'dap-ui-repl-process
     "l" #'dap-ui-locals
     "r" #'dap-ui-restart-frame
     "w" #'dap-ui-hide-many-windows
     "W" #'dap-ui-show-many-windows
     "s" #'dap-ui-session
     "b" #'dap-ui-breakpoints
     "h" #'dap-hydra)))
  (let ((file-path (expand-file-name
     ".dap-env.el" doom-private-dir)))
    (when (file-exists-p file-path)
      (load-file file-path)))
  (dap-mode 1)
  :ensure t)

(use-package! dap-python
  :after dap-mode
  ;; :config
  ;; (dap-python-setup)
  :ensure t)

(use-package! dap-cpptools
  :after dap-mode
  ;; :config
  ;; (dap-cpptools-setup)
  :ensure t)

(use-package! dap-gdb-lldb
  :after dap-mode
  :config
  (dap-gdb-lldb-setup)
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
