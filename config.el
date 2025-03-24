;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Martin Liu"
      user-mail-address "hflhmartin@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code"
                           :size 20)
      doom-variable-pitch-font (font-spec :family "Fira Code" :size 14))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; Configs
(setq
 byte-compile-warnings '(not docstrings obsolete)
 doom-localleader-key ","
 doom-localleader-alt-key "M-,"
 ;; mac
 mac-command-modifier 'meta
 mac-option-modifier 'super
 ;; avy keys for jump buffer
 avy-keys (number-sequence ?a ?z)
 frog-menu-avy-keys (number-sequence ?a ?z)
 ;; dired
 dired-dwim-target t
 ;; project
 projectile-project-search-path '("~/src")
 ;; ein
 ein:output-area-inlined-images t
 ein:worksheet-enable-undo t
 ;; for jupyterlab > 3.0, this should be set to "notebook" when using notebook
 ein:jupyter-server-use-subcommand "server"
 ;; others
 create-lockfiles nil
 debug-on-error t)
(menu-bar-mode -1)

;;; Keys
(global-set-key (kbd "M-`") 'evil-escape)
(global-set-key (kbd "M-p") 'evil-jump-backward)
(global-set-key (kbd "M-n") 'evil-jump-forward)
(global-set-key (kbd "C-M-e") 'end-of-defun)
(global-set-key (kbd "C-M-b") 'beginning-of-defun)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c C-g") 'evil-force-normal-state)
(map! :leader
      :desc "just-one-space"
      "m o" #'just-one-space)
(map! :leader
      :desc "frog-jump-buffer"
      "." #'frog-jump-buffer)
(map! :leader
      :desc "lsp-ui-doc-glance"
      "c g" #'lsp-ui-doc-glance)
(map! :leader
      :desc "lsp-describe-thing-at-point"
      "c h" #'lsp-describe-thing-at-point)

;;; Hooks
;; enable undo when evil
(after! evil-mode
  (add-hook ’evil-local-mode-hook ’turn-on-undo-tree-mode))
(global-evil-matchit-mode 1)

;; automatically save buffers when focus out or switch
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; magit
;; code review
(setq
 code-review-auth-login-marker 'forge
 forge-database-connector 'sqlite-builtin
 code-review-fill-column 80
 code-review-new-buffer-window-strategy #'switch-to-buffer
 )
(add-hook 'code-review-mode-hook
          (lambda ()
            ;; include *Code-Review* buffer into current workspace
            (persp-add-buffer (current-buffer))))
;; set auth source for magit forge
(after! auth-source
  (pushnew! auth-sources "~/.netrc"))

;; autoload python virtual environments
;; https://github.com/jorgenschaefer/pyvenv/issues/51#issuecomment-474785730
(defun pyvenv-autoload ()
  "Activates pyvenv version if venv directory exists."
  (f-traverse-upwards
   (lambda (path)
     (let ((venv-path (f-expand "venv" path))
           (dotvenv-path (f-expand ".venv" path)))
       (cond ((f-exists? venv-path) (pyvenv-activate venv-path))
             ((f-exists? dotvenv-path) (pyvenv-activate dotvenv-path)))))))
(add-hook 'python-mode-hook 'pyvenv-autoload)
(add-hook 'projectile-after-switch-project-hook 'pyvenv-autoload)
;(add-hook 'doom-switch-buffer-hook 'pyvenv-autoload)

;; lsp
(after! lsp-mode
  (setq lsp-file-watch-ignored-directories (append
                                            '("[/\\\\]\\.venv\\'"
                                              "[/\\\\]\\venv\\'"
                                              "[/\\\\]\\.aider.*\\'"
                                              "[/\\\\]\\.tmp\\'"
                                              "[/\\\\]\\.*cache\\'")
                                            lsp-file-watch-ignored-directories)
        lsp-file-watch-threshold 2000
        ))

;; dap mode for debugger
(after! dap-mode
  (setq dap-python-debugger 'debugpy
        ;; add `justMyCode: false' to debugpy, so that it will step in libirary code
        dap-debug-template-configurations (mapcar
                                           (lambda (conf)
                                             (progn
                                               (add-to-list 'conf :justMyCode t)
                                               (add-to-list 'conf :json-false t))
                                             )
                                           dap-debug-template-configurations)
        ))

; override dap python function to ensure venv works
(after! dap-python
  (defun dap-python--pyenv-executable-find (command) (executable-find command)))

; Astro
;; use rjsx-mode for .astro files
(add-to-list 'auto-mode-alist '("\\.astro\\'" . rjsx-mode))

; Rust
(after! dap-mode
  (map! :map dap-mode-map
        :leader
        :prefix ("d" . "dap")
        ;; basics
        :desc "dap next"          "n" #'dap-next
        :desc "dap step in"       "i" #'dap-step-in
        :desc "dap step out"      "o" #'dap-step-out
        :desc "dap continue"      "c" #'dap-continue
        :desc "dap hydra"         "h" #'dap-hydra
        :desc "dap debug restart" "r" #'dap-debug-restart
        :desc "dap debug"         "s" #'dap-debug

        ;; debug
        :prefix ("dd" . "Debug")
        :desc "dap debug recent"  "r" #'dap-debug-recent
        :desc "dap debug last"    "l" #'dap-debug-last

        ;; eval
        :prefix ("de" . "Eval")
        :desc "eval"                "e" #'dap-eval
        :desc "eval region"         "r" #'dap-eval-region
        :desc "eval thing at point" "s" #'dap-eval-thing-at-point
        :desc "add expression"      "a" #'dap-ui-expressions-add
        :desc "remove expression"   "d" #'dap-ui-expressions-remove

        :prefix ("db" . "Breakpoint")
        :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
        :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
        :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
        :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)

  ;; (require 'dap-codelldb)
  ;; (dap-codelldb-setup)
  ;; (dap-register-debug-template "Rust::CodeLLDB Run Configuration"
  ;;                              (list :type "lldb"
  ;;                                    :request "launch"
  ;;                                    :name "CodeLLDB::Run"
  ;;                                    :program ""
  ;;                                    :cargo (list :args '("test" "--no-run"))))
  )

;; markdown
(map! :map markdown-mode-map
      :localleader
      :desc "follow-link-at-point" "f" #'markdown-follow-link-at-point)

;; org
(after! org (setq org-log-done "time"
                  org-default-notes-file "~/Google Drive/My Drive/doc/GTD.org"
                  org-agenda-files '("~/Google Drive/My Drive/doc/")
                  org-tag-alist '(("@work" . ?w) ("@me" . ?m))
                  org-capture-templates '(("t" "TODO" entry (file+headline "" "Tasks") "* TODO %?\n %i\n")
                                          ("n" "NOTE" entry (file+headline "" "Notes") "* NOTE - %?\n %i\n %a")
                                          ("j" "Journal" entry (file+datetree "~/Google Drive/My Drive/doc/journal.org")
                                           "* %U - %^{heading}\n%?"))
                  org-roam-directory "~/martin/code/my/learning/notes/roam"
                  org-roam-graph-executable "neato"
                  org-roam-database-connector 'sqlite-builtin ; use emacs 29 built-in sqlite
                  org-startup-with-inline-images t
                  ))
;; fragtog auto do latex preview in org
(after! org (add-hook 'org-mode-hook 'org-fragtog-mode))

;; Github Copilot
;; accept completion from copilot and fallback to company
;; (use-package! copilot
;;   :hook ((prog-mode . copilot-mode)
;;          (org-mode . copilot-mode)
;;          (markdown-mode . copilot-mode)
;;          (gfm-mode . copilot-mode))
;;   :bind (("s-TAB" . 'copilot-accept-completion-by-word)
;;          ("s-<tab>" . 'copilot-accept-completion-by-word)
;;          :map copilot-completion-map
;;          ("<tab>" . 'copilot-accept-completion)
;;          ("TAB" . 'copilot-accept-completion)))

;; Codeium
;; we recommend using use-package to organize your init.el
(use-package! codeium
    ;; if you use straight
    ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
    ;; otherwise, make sure that the codeium.el file is on load-path

    :init
    ;; use globally
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;; aidermacs
(use-package! aidermacs
  :config
  (setq aidermacs-backend 'vterm)
  (setq aidermacs-show-diff-after-change nil)
  (setq aidermacs-vterm-multiline-newline-key "S-<return>")
  (setq aidermacs-extra-args '("--no-show-model-warnings"))
  :custom
  ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "litellm_proxy/claude-3-7-sonnet")
  (aidermacs-architect-model "litellm_proxy/claude-3-7-sonnet"))
(after! aidermacs
  (map! :leader
        :desc "aidermacs-transient-menu" "m a" #'aidermacs-transient-menu
        ))

;; leetcode
(setq leetcode-prefer-language "python3")
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/martin/code/my/leetcode/leetcode/")


;; pdf view
(defun pdf-view-zoom-center ()
  "Zoom the pdf view and center the view."
  (interactive)
  (pdf-view-fit-width-to-window)
  (pdf-view-enlarge 1.25)
  (pdf-view-center-in-window))
(after! pdf-tools
  (map! :leader
        :desc "zoom center"
        "m c" #'pdf-view-zoom-center))
