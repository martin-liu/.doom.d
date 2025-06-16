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
 projectile-project-search-path '("~/martin/code/nand")
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

;; Python
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

;; Python Debug
(defun dape-pytest-one-debug ()
  (interactive)
  (when-let* ((node (treesit-node-at (point)))
              (func-node (treesit-node-top-level node "function_definition"))
              (func-name (treesit-node-text (treesit-node-child-by-field-name func-node "name") t))
              (test-path (concat "::" func-name)))

    ;; if in class, add class name
    (let ((class-node (treesit-node-top-level node "class_definition")))
      (when class-node
        (let ((class-name (treesit-node-text (treesit-node-child-by-field-name class-node "name") t)))
          (setq test-path (concat "::" class-name test-path)))))

    (let* ((cwd (dape-cwd))
           (rel-path (file-relative-name buffer-file-name cwd))
           (full-test-path (concat rel-path test-path))
           (conf (alist-get 'debugpy-module dape-configs)))
      (cl-callf plist-put conf :module "pytest")
      (cl-callf plist-put conf :args (vector full-test-path))
      (cl-callf plist-put conf :cwd cwd)
      (dape conf))))
(map! :leader
      :desc "dape-pytest-one-debug"
      "m d" #'dape-pytest-one-debug)

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

; Astro
;; use rjsx-mode for .astro files
(add-to-list 'auto-mode-alist '("\\.astro\\'" . rjsx-mode))

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
                                          ("p" "Post" entry (file+datetree "~/Google Drive/My Drive/doc/posts.org") "* %U - %^{heading}\n%?")
                                          ("j" "Journal" entry (file+datetree "~/Google Drive/My Drive/doc/journal.org")
                                           "* %U - %^{heading}\n%?"))
                  org-roam-directory "~/martin/code/my/learning/notes/roam"
                  org-roam-graph-executable "neato"
                  org-roam-database-connector 'sqlite-builtin ; use emacs 29 built-in sqlite
                  org-startup-with-inline-images t
                  ))
;; fragtog auto do latex preview in org
(after! org (add-hook 'org-mode-hook 'org-fragtog-mode))

;; AI code completion
(use-package! minuet
    :bind
    (("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
     ("C-c m" . #'minuet-configure-provider)
     :map minuet-active-mode-map
     ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
     ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
     ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
     ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
     ;; Accept the first line of completion, or N lines with a numeric-prefix:
     ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
     ("M-a" . #'minuet-accept-suggestion-line)
     ("M-e" . #'minuet-dismiss-suggestion))

    :init
    ;; if you want to enable auto suggestion.
    ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
    (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)
    (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

    :config
    ;; You can use M-x minuet-configure-provider to interactively configure provider and model
    (setq minuet-provider 'openai-compatible)
    (plist-put minuet-openai-compatible-options :end-point "https://llmgateway.app.nand.ai/v1/chat/completions")
    (plist-put minuet-openai-compatible-options :api-key "LITELLM_PROXY_API_KEY")
    (plist-put minuet-openai-compatible-options :model "gpt-4.1-mini")
    (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 256)
    (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9))

    ;; For Evil users: When defining `minuet-ative-mode-map` in insert
    ;; or normal states, the following one-liner is required.

    ;; (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)

    ;; This is *not* necessary when defining `minuet-active-mode-map`.

    ;; To minimize frequent overhead, it is recommended to avoid adding
    ;; `evil-normalize-keymaps` to `minuet-active-mode-hook`. Instead,
    ;; bind keybindings directly within `minuet-active-mode-map` using
    ;; standard Emacs key sequences, such as `M-xxx`. This approach should
    ;; not conflict with Evil's keybindings, as Evil primarily avoids
    ;; using `M-xxx` bindings.

;; aidermacs
(use-package! aidermacs
  :config
  (setq aidermacs-backend 'vterm)
  (setq aidermacs-show-diff-after-change nil)
  (setq aidermacs-vterm-multiline-newline-key "S-<return>")
  (setq aidermacs-extra-args '("--timeout=1200 --thinking-tokens 32k --model-metadata-file=~/.aider.model.metadata.json --no-show-model-warnings --no-auto-lint"))
  :custom
  ; See the Configuration section below

  ;; (aidermacs-default-chat-mode 'architect)
  ;; (aidermacs-default-model "litellm_proxy/claude-4-sonnet")
  ;; (aidermacs-weak-model "litellm_proxy/gpt-4.1")
  ;; (aidermacs-editor-model "litellm_proxy/claude-4-sonnet")
  ;; (aidermacs-architect-model "litellm_proxy/claude-4-sonnet-thinking"))

  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "litellm_proxy/gpt-4.1")
  (aidermacs-weak-model "litellm_proxy/gpt-4o")
  (aidermacs-editor-model "litellm_proxy/gpt-4.1")
  (aidermacs-architect-model "litellm_proxy/o3"))

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
