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
 ;; magit
 forge-database-connector 'sqlite-builtin
 ;; project
 projectile-project-search-path '("~/src")
 ;; ein
 ein:output-area-inlined-images t
 ein:worksheet-enable-undo t
 ;; others
 create-lockfiles nil
 debug-on-error t)
(menu-bar-mode -1)

;;; Keys
(global-set-key (kbd "M-`") 'evil-escape)
(global-set-key (kbd "M-p") 'evil-jump-backward)
(global-set-key (kbd "M-n") 'evil-jump-forward)
(global-set-key (kbd "C-M-f") 'end-of-defun)
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

;; set auth source
(after! auth-source
  (pushnew! auth-sources "~/.netrc"))

;; autoload python virtual environments
;; https://github.com/jorgenschaefer/pyvenv/issues/51#issuecomment-474785730
(defun pyvenv-autoload ()
  "Activates pyvenv version if venv directory exists."
  (f-traverse-upwards
   (lambda (path)
     (let ((venv-path (f-expand "venv" path)))
       (when (f-exists? venv-path)
         (pyvenv-activate venv-path))))))
(add-hook 'python-mode-hook 'pyvenv-autoload)
(add-hook 'projectile-after-switch-project-hook 'pyvenv-autoload)
;(add-hook 'doom-switch-buffer-hook 'pyvenv-autoload)

;; lsp
(after! lsp-mode
  (setq lsp-file-watch-ignored-directories (cons
                                            "[/\\\\]\\.venv\\'"
                                            (cons
                                             "[/\\\\]\\venv\\'"
                                             lsp-file-watch-ignored-directories))
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
                  org-default-notes-file "~/GoogleDrive/doc/GTD.org"
                  org-agenda-files '("~/GoogleDrive/doc/")
                  org-tag-alist '(("@work" . ?w) ("@me" . ?m))
                  org-capture-templates '(("t" "TODO" entry (file+headline "" "Tasks") "* TODO %?\n %i\n")
                                          ("n" "NOTE" entry (file+headline "" "Notes") "* NOTE - %?\n %i\n %a")
                                          ("j" "Journal" entry (file+datetree "~/GoogleDrive/doc/journal.org")
                                           "* %U - %^{heading}\n%?"))
                  org-roam-directory "~/martin/code/my/learning/notes/roam"
                  org-roam-graph-executable "neato"
                  org-roam-database-connector 'sqlite-builtin ; use emacs 29 built-in sqlite
                  ))
;; fragtog auto do latex preview in org
(after! org (add-hook 'org-mode-hook 'org-fragtog-mode))

;; Github Copilot
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("s-TAB" . 'copilot-accept-completion-by-word)
         ("s-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

;; leetcode
(setq leetcode-prefer-language "python3")
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/martin/code/my/leetcode/src/main/python/")
