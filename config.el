;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (setq doom-font (font-spec :family "Symbols Nerd Font Mono" :size 14.0))
;; (setq doom-fontdoom-font (font-spec :family "Fantasque Sans Mono" :size 27.0))
;; (setq doom-font (font-spec :family "Fantasque Sans Mono" :size 27.0))


;; MY CUSTOM CONFIG


;; Expand the application to full screen on first launch
;; (add-hook 'window-setup-hook #'toggle-frame-fullscreen)
(add-hook 'window-setup-hook #'toggle-frame-maximized)

;; Set font size
(setq doom-font (font-spec :size 15))

;; Interrupt command
;; (define-key vterm-mode-map (kbd "C-c C-c") 'vterm--self-ctrl-c)

;; ;; Configure jumping
;; (map! :leader
;;       (:prefix ("j" . "jump")
;;        :desc "window jump" "w" #'ace-window
;;        :desc "line jump"   "l" #'ace-jump-line-mode
;;        :desc "word jump"   "j" #'ace-jump-word-mode))

;; Configure Window Jump
(use-package ace-window
  :ensure t
  :config
  (map! :leader
        (:prefix ("j" . "jump")
         :desc "window jump" "w" #'ace-window)))

;; Configure Text Jumping
(use-package ace-jump-mode
  :ensure t
  :config
  (map! :leader
        (:prefix ("j" . "jump")
         :desc "line jump" "l" #'ace-jump-line-mode
         :desc "word jump" "j" #'ace-jump-word-mode)))

;; Translate to Rusian
(use-package go-translate
  :ensure t
  :custom ((gt-translate-list '(("en" "ru")
                                ("ru" "en"))))
  :config
  (defun translate-it (engine render)
    (interactive)
    (setq gt-default-translator
          (gt-translator
           :taker (gt-taker :langs '(en ru))
           :engines engine
           :render render))
    (gt-do-translate))
  (map! :leader
      (:prefix ("l" . "translate")
         (:prefix ("g" . "google")
          :desc "to overlay"  "o" (lambda () (interactive) (translate-it gt-google-engine gt-overlay-render))
          :desc "to buffer"   "b" (lambda () (interactive) (translate-it gt-google-engine gt-buffer-render))
          :desc "to insert"   "i" (lambda () (interactive) (translate-it gt-google-engine gt-insert-render))
          :desc "to killring" "c" (lambda () (interactive) (translate-it gt-google-engine gt-kill-ring-render)))
         (:prefix ("b" . "bing")
          :desc "to overlay"  "o" (lambda () (interactive) (translate-it gt-bing-engine gt-overlay-render))
          :desc "to buffer"   "b" (lambda () (interactive) (translate-it gt-bing-engine gt-buffer-render))
          :desc "to insert"   "i" (lambda () (interactive) (translate-it gt-bing-engine gt-insert-render))
          :desc "to killring" "c" (lambda () (interactive) (translate-it gt-bing-engine gt-kill-ring-render))))))

;; Disable slowdown when holding keys
(setq auto-repeat-fast nil)

;; Bind C-j/C-k to scroll one line down/up in normal mode.
(define-key evil-normal-state-map (kbd "M-j") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "M-k") 'evil-scroll-up)

;; Configure DAP & UI Mode
(use-package dap-mode
  :ensure t
  :config
  ;; (require 'dap-lldb)
  ;; (require 'dap-gdb)
  (require 'dap-gdb-lldb)
  (require 'dap-cpptools)
  (require 'dap-hydra)
  (setq dap-auto-configure-mode t
        ;; dap-auto-show-output nil
        ;; gdb-many-windows nil
        dap-auto-configure-features '(sessions locals))
  ;; (dap-tooltip-mode t)
  ;; (tooltip-mode t)
  ;; (gdb-many-windows t)
  ;; (dap-ui-controls-mode t)
  (dap-ui-mode t)
  (dap-gdb-lldb-setup)
  (dap-cpptools-setup)
  ;; (load-theme 'doom-tomorrow-night t)
  (add-hook 'c-mode-common-hook 'dap-mode)
  ;; (add-hook 'dap-stopped-hook 'dap-hydra)
  ;; (add-hook 'dap-stopped-hook
        ;; #'(lambda (_) (dap-hydra/body)))
  ;; (add-hook 'dap-stopped-hook
  ;;       (lambda (_) (call-interactively #'dap-hydra)))
  ;; (evil-define-key 'normal dap-mode-map
    ;; (kbd "SPC d g") 'dap-goto-breakpoint
    ;; (kbd "SPC d c") 'dap-continue
    ;; (kbd "C-j") 'dap-next
    ;; (kbd "C-k") 'dap-step-in
    ;; (kbd "C-h") 'dap-step-out
    ;; (kbd "SPC d r") 'dap-debug-restart
    ;; (kbd "SPC d e") 'dap-disconnect
    ;; (kbd "SPC d R") 'dap-debug-last
    ;; (kbd "SPC d b") 'dap-breakpoint-toggle
    ;; (kbd "SPC d B") 'dap-breakpoint-condition
    ;; (kbd "SPC d D") 'dap-breakpoint-delete-all)
  )

(fringe-mode 5)

;; (setq fringes-outside-margins t)
;; (setq dap-ui-use-fringe t)

(dap-register-debug-template
 "cpptools::samba"
 (list :type "cppdbg"
       :request "launch"
       :program "/app/inno-samba/sbin/samba"
       :cwd "/home/user/Work/samba"
       :MIMode "gdb"
       :targetArchitecture "x86_64"
       :miDebuggerPath "sugdb"
       :miDebuggerArgs (string-join '("--silent") " ")
       ;; :useExtendedRemote t
       ;; :miDebuggerServerAddress "localhost:1234"
       ;; :postRemoteConnectCommands
       :stopAtEntry t
       :args [
           "--foreground"
           "--debuglevel=10"
           "--no-process-group"
           "--debug-stdout"]
       ;; :setupCommands [{
       ;;     :description "Enable pretty printing"
       ;;     :text "-enable-pretty-printing"
       ;; } {
       ;;     :description "Set break at main function"
       ;;     :text "set follow-fork-mode child"
       ;; }]
              ;; (:description "Enable pretty-printingb"
              ;;  :text "-enable-pretty-printing")
              ;; (:description "Set Disassembly Flavor to Intel"
              ;;  :text "-gdb-set disassembly-flavor intel")
       :setupCommands [
           (:description "Stop at main function"
            :text "break main")
           (:description "Don't detach forkeds"
            :text "set detach-on-fork off")
           (:description "Follow child process"
            :text "set follow-fork-mode child")]))

(dap-register-debug-template
 "gdb::samba"
 (list :type "gdb"
       :request "launch"
       :target "samba"
       :cwd "/app/inno-samba/sbin"
       :args [
           "--foreground"
           "--debuglevel=10"
           "--no-process-group"
           "--debug-stdout"]
       :stopAtEntry t
       :setupCommands [
              (:description "Stop at main function"
               :text "break main")
              (:description "Don't detach forkeds"
               :text "set detach-on-fork off")
              ;; (:description "Enable pretty-printingb"
              ;;  :text "-enable-pretty-printing")
              ;; (:description "Set Disassembly Flavor to Intel"
              ;;  :text "-gdb-set disassembly-flavor intel")
              (:description "Follow child process"
               :text "set follow-fork-mode child")]))



;; ;; Template for DDB Inno-Samba
;; (dap-register-debug-template
;;   "gdb:samba - debug minimal"
;;    (list :type "gdb"
;;          :request "launch"
;;          :name "gdb:samba"
;;          :gdbpath "rust-gdb"
;;          :target nil
;;          :cwd nil))

;; ;; Template for DDB Inno-Samba
;; (dap-register-debug-template
;;   "gdb:samba - debug template"
;;    (list :type "gdb"
;;          :request "launch"
;;          :name "gdb:samba"
;;          :program "samba"
;;          :args '("--foreground"
;;                  "--no-process-group"
;;                  "--debug-stdout"
;;                  "--debuglevel=10")
;;          :cwd "/app/inno-samba/sbin"
;;          :MIMode "gdb"
;;          :miDebuggerPath "/usr/bin/gdb"
;;          ;; ::gdbpath "/usr/bin/gdb"
;;          :environment []
;;          :setupCommands '(
;;              (:description "Enable pretty printing"
;;               :text "-enable-pretty-printing")
;;              (:description "Disassembly Flavor to Intel"
;;               :text "-gdb-set disassembly-flavor intel")
;;              (:description "Follow child process"
;;               :text "set follow-fork-mode child")
;;              (:text "set target-async off"))
;;          :stop-on-entry t))

;; ;; Template for CPPGDB Inno-Samba
;; (dap-register-debug-template
;;   "cppgdb:samba - debug template"
;;    (list :type "cppgdb"
;;          :request "launch"
;;          :name "cppgdb:samba"
;;          :program "/app/inno-samba/sbin/samba"
;;          :args '("--foreground"
;;                  "--no-process-group"
;;                  "--debug-stdout"
;;                  "--debuglevel=10")
;;          :cwd (lsp-workspace-root)
;;          :externalConsole nil
;;          :MIMode "gdb"
;;          :miDebuggerPath "gdb"
;;          :environment []
;;          :setupCommands nil
;;          :target nil
;;          :stopOnEntry t))


;; (dap-register-debug-template
;;   "Debug Samba with Breakpoint on Start"
;;    (list :type "gdb"
;;          :request "launch"
;;          :name "Samba with Breakpoint"
;;          :program "/app/inno-samba/sbin/samba"
;;          :args '("--foreground" "--no-process-group" "--debug-stdout" "-d10")
;;          :cwd (file-name-directory "/app/inno-samba/sbin")
;;          :stop-on-entry t
;;          :setupCommands '(
;;              (:text "-enable-pretty-printing"
;;               :description "Enable pretty printing")
;;              (:text "set follow-fork-mode child"
;;               :description "Follow child process")
;;              (:text "set detach-on-fork off"
;;               :description "Don't detach forked processes")
;;              (:text "break main"
;;               :description "Stop at main function"))))

;; (dap-register-debug-template
;;    "Inno-Samba"
;;    (list :type "gdb"
;;          :request "launch"
;;          :program "/app/inno-samba/sbin/samba"
;;          :MIMode "gdb"
;;          :miDebuggerPath "gdb"
;;          :args '("--foreground"
;;                  "--debuglevel=10"
;;                  "--no-process-group"
;;                  "--debug-stdout")
;;          :cwd (file-name-directory "/app/inno-samba/sbin")
;;          :stop-on-entry t
;;          :setupCommands [
;;                 (:description "Enable pretty-printing for gdb"
;;                  :text "-enable-pretty-printing"
;;                  :ignoreFailures t)
;;                 (:description "Stop at main function"
;;                  :text "Break on main")
;;                 (:description "Don't detach forked processes"
;;                  :text "set detach-on-fork off")
;;                 (:description "Follow child process"
;;                  :text "set follow-fork-mode child")
;;                 (:description "Set Disassembly Flavor to Intel"
;;                  :text "-gdb-set disassembly-flavor intel"
;;                  :ignoreFailures t)]))


;; (dap-register-debug-template
;;    "Inno-Samba"
;;    (list :type "gdb"
;;          :request "launch"
;;          :cwd "/app/inno-samba/sbin"
;;          :target "/usr/bin/sudo /app/inno-samba/sbin/samba --foreground --debuglevel=10 --no-process-group --debug-stdout"
;;          ;; :MIMode "gdb"
;;          ;; :miDebuggerPath "gdb"
;;          ;; :args '("--foreground"
;;          ;;         "--debuglevel=10"
;;          ;;         "--no-process-group"
;;          ;;         "--debug-stdout")
;;          ;; :cwd (file-name-directory "/app/inno-samba/sbin")
;;          ;; :stop-on-entry t
;;          ;; :setupCommands [
;;          ;;        (:description "Enable pretty-printing for gdb"
;;          ;;         :text "-enable-pretty-printing"
;;          ;;         :ignoreFailures t)
;;          ;;        (:description "Stop at main function"
;;          ;;         :text "Break on main")
;;          ;;        (:description "Don't detach forked processes"
;;          ;;         :text "set detach-on-fork off")
;;          ;;        (:description "Follow child process"
;;          ;;         :text "set follow-fork-mode child")
;;          ;;        (:description "Set Disassembly Flavor to Intel"
;;          ;;         :text "-gdb-set disassembly-flavor intel"
;;          ;;         :ignoreFailures t)]
;;          ))

;; (dap-register-debug-template
;;    "Inno-Samba"
;;    (list :type "cppdbg"
;;          :request "launch"
;;          :cwd "/app/inno-samba/sbin"
;;          :program "/app/inno-samba/sbin/samba --foreground --debuglevel=10 --no-process-group --debug-stdout"
;;          :MIMode "gdb"
;;          ;; :miDebuggerPath "gdb"
;;          ;; :args '("--foreground"
;;          ;;         "--debuglevel=10"
;;          ;;         "--no-process-group"
;;          ;;         "--debug-stdout")
;;          ;; :cwd (file-name-directory "/app/inno-samba/sbin")
;;          ;; :stop-on-entry t
;;          ;; :setupCommands [
;;          ;;        (:description "Enable pretty-printing for gdb"
;;          ;;         :text "-enable-pretty-printing"
;;          ;;         :ignoreFailures t)
;;          ;;        (:description "Stop at main function"
;;          ;;         :text "Break on main")
;;          ;;        (:description "Don't detach forked processes"
;;          ;;         :text "set detach-on-fork off")
;;          ;;        (:description "Follow child process"
;;          ;;         :text "set follow-fork-mode child")
;;          ;;        (:description "Set Disassembly Flavor to Intel"
;;          ;;         :text "-gdb-set disassembly-flavor intel"
;;          ;;         :ignoreFailures t)]
;;          ))


;; (use-package dap-gdb-lldb
;;   :after dap-mode
;;   :demand
;;   :custom
;;   (dap-gdb-lldb-extension-version
;;    (+github-latest-release "WebFreak001/code-debug" "0.27.0")))

;; (use-package dap-cpptools
;;   :after dap-mode
;;   :demand
;;   :custom
;;   (dap-cpptools-extension-version
;;    (+github-latest-release "microsoft/vscode-cpptools" "1.18.5")))


;; (use-package dap-hydra
;;   :defer t
;;   :after dap-mode)

;; (use-package dap-cpptools
;;   :defer t
;;   :hook ((c-mode c++-mode) . (lambda () (require 'dap-cpptools)))
;;   :after dap-mode)

;; (use-package dap-gdb-lldb
;;   :after dap-mode
;;   :hook ((c-mode c++-mode) . (lambda () (require 'dap-gdb-lldb))))

;; (use-package dap-lldb
;;   :after dap-mode
;;   :hook ((c-mode c++-mode) . (lambda () (require 'dap-lldb)))
;;   :config (progn
;;             (setq
;;              dap-lldb-debug-program
;;              `(,(expand-file-name "~/.emacs.d/llvm-project/lldb/build/bin/lldb-vscode")))

;;             (defun dap-cppdbg-gdb-attach (file)
;;               (interactive "fPath to running executable: ")
;;               (let ((pid (shell-command-to-string (format "pidof -s %s" (f-base file)))))
;;                 (progn
;;                   (message (format "%s" pid))
;;                   (dap-debug (list :type "cppdbg"
;;                                    :request "attach"
;;                                    :program (expand-file-name file)
;;                                    :MIMode "gdb"
;;                                    :processId pid
;;                                    :name "cpptools::Attach GDB")))))

;;             (defun dap-cppdbg-lldb-attach (file)
;;               (interactive "fPath to running executable: ")
;;               (let ((pid (shell-command-to-string (format "pidof %s" (f-base file)))))
;;                 (progn
;;                   (message (format "%s" pid))
;;                   (dap-debug (list :type "cppdbg"
;;                                    :request "attach"
;;                                    :program (expand-file-name file)
;;                                    :MIMode "lldb"
;;                                    :processId pid
;;                                    :name "cpptools::Attach LLDB")))))

;;             (defun dap-lldb-attach (file)
;;               (interactive "fPath to running executable: ")
;;               (let ((pid (shell-command-to-string (format "pidof %s" (f-base file)))))
;;                 (progn
;;                   (message (format "%s" pid))
;;                   (dap-debug (list :type "lldb-vscode"
;;                                    :request "attach"
;;                                    :program (expand-file-name file)
;;                                    :pid pid
;;                                    :stopOnEntry nil
;;                                    :name "LLDB::Attach")))))))


;; (dap-register-debug-template "cpptools::Samba"
;;     (list :type "cppdbg"
;;           :request "launch"
;;           :name "cpptools::Run Configuration"
;;           :MIMode "gdb"
;;           :program "/app/inno-samba/sbin/samba"
;;           :stopOnEntry t
;;           :cwd "/app/inno-samba/sbin"))

;;  ;; --foreground --debuglevel=10 --no-process-group --debug-stdout



;; (dap-register-debug-template
;;    "gdb::dap::samba"
;;     (list :type "gdb"
;;           :request "launch"
;;           :target "/app/inno-samba/sbin/samba"
;;           :gdbpath "/usr/bin/gdb"
;;           ;; :args ["--foreground" "--debuglevel=10" "--no-process-group" "--debug-stdout"]
;;           ;; :stopOnEntry t
;;           :cwd "/app/inno-samba/sbin"))


;; (setq evil-want-integration t)


;; (defun my/activate-english-input ()
;;   "Switch to English layout when certain keys are pressed."
;;   (interactive)
;;   (set-input-method "english"))
;; (add-hook 'minibuffer-setup-hook 'my/activate-english-input)

(setq evil-escape-unordered-key-sequence t)

;; (use-package unimpaired-mode
;;   :ensure t
;;   :config
;;   (unimpaired-mode 1))
