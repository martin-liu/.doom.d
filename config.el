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
 doom-localleader-key ","
 doom-localleader-alt-key "M-,"
 ;; mac
 mac-command-modifier 'meta
 mac-option-modifier 'super
 ;; avy keys for jump buffer
 avy-keys (number-sequence ?a ?z)
 frog-menu-avy-keys (number-sequence ?a ?z)
 ;; project
 projectile-project-search-path '("~/src")
 ;; others
 create-lockfiles nil
 debug-on-error t)

;;; Keys
(global-set-key (kbd "M-`") 'evil-escape)
(global-set-key (kbd "M-p") 'evil-jump-backward)
(global-set-key (kbd "M-n") 'evil-jump-forward)
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
;; automatically save buffers when focus out or switch
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

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

;; dap mode for debugger
(after! dap-mode
  (setq dap-python-debugger 'debugpy))
; override dap python function to ensure venv works
(after! dap-python
  (defun dap-python--pyenv-executable-find (command) (executable-find command)))
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
        :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message))

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
                                           "* %U - %^{heading}\n%?"))))
