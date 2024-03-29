;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Miller"
      user-mail-address "millejoh@mac.com")

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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "Hack" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; Other themes that seem to work:
;;  doom-nord
;;  doom-flatwhite
;;  doom-palenite
;;  doom-plaindark
;;  doom-opera

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/Journal/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Because, in the end, paredit was all I ever needed. See
;; https://github.com/luxbock/evil-cleverparens for documentation.
(add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)

(use-package! avy
  :commands (avy-goto-word-1))

(use-package! nano-theme)

(use-package! page-break-lines
  :config
  (global-page-break-lines-mode 1))

(use-package! jupyter
  :demand t
  :after (:all org python))

(use-package! edit-server)

(use-package! org-modern
  :config (global-org-modern-mode))

;; (use-package! symex
;;   :custom (symex-modal-backend 'hydra)
;;   :config
;;   (symex-initialize)
;;   (map! (:prefix "z" :n ";" 'symex-mode-interface )))

(setq +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info"))
(setq +python-jupyter-repl-args '("--simple-prompt"))

;;; Custom leader maps
;;;
(map! :leader :desc "M-x" "SPC" 'execute-extended-command
      :leader :desc "Jump/join/split" "j" '(:ignore t :which-key "jump/join/split")
      :leader :desc "Jump to line" "jl" 'avy-goto-line
      :leader :desc "Jump to word" "jw" 'avy-goto-word-1
      :leader :desc "Jumpt to char" "jc" 'avy-goto-char
      :leader :desc "Close window or workspace" "w0" '+workspace/close-window-or-workspace)

;;; Find a way to make this common to my custom-emacs config, as of the moment
;;; there is too much duplication.
(defun config-path (path)
  (if (locate-library "portacle")
      (portacle-path (concat "config/config.d/" path))
    (concat "~/custom-emacs/" path)))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  `((emacs-lisp . t)
;;    (lisp . t)
;;    (python . t)
;;    (jupyter . t)))


;; (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(load! "elisp.el")

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(general-auto-unbind-keys :off)
(remove-hook 'doom-after-init-modules-hook #'general-auto-unbind-keys)

;;;  Fix for emacs-jupyter per issue #366, though may still see issues since
;; ansi-color--find-face has been renamed to ansi-color--face-vec-face in more recent
;; versions of emacs.
;;
(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(setq-default mode-line-format
              ;; ...
               '(:eval (when (featurep 'lispyville)
                         (lispyville-mode-line-string)))
              ;; ...
              )

(add-hook 'org-babel-after-execute-hook #'display-ansi-colors)

(add-to-list 'Info-directory-list "~/.local/share/info")

;;; Better column display in org.
;;; per: https://stackoverflow.com/questions/7864985/org-mode-make-links-clickable-in-column-view
;;;
(defun my-column-display-value-transformer (column-title value)
  "Modifies the value to display in column view."
  (when (and (string-prefix-p "[[" value)
             (string-suffix-p "]]" value))
    (string-match org-link-bracket-re value)
    (let ((target (match-string 1 value))
          (descr (match-string 2 value)))
      (setq value (or descr target))
      ;; put the suitable faces:
      (put-text-property 0 (length value) 'face 'org-link value)
      (put-text-property 0 (length value) 'mouse-face 'highlight value)
      ;; help echo:
      (put-text-property 0 (length value) 'help-echo
                         (format "LINK: %s" target) value)
      ;; put a local keymap:
      (put-text-property 0 (length value) 'keymap
                         '(keymap (mouse-1 . org-columns-open-link)) value)
      value)))

(setq org-columns-modify-value-for-display-function
      #'my-column-display-value-transformer)

;;;
;;;  Configure org capture for common service actions
;;;

(setq +service-forecast-file "/mnt/c/Users/E341194/OneDrive - Honeywell/Customers/Journals/roam/20220622102420-service_forecast.org")
(setq +service-forecast-template "/mnt/c/Users/E341194/OneDrive - Honeywell/Customers/Journals/roam/ForecastTemplate.org")
(setq +service-case-file "/mnt/c/Users/E341194/OneDrive - Honeywell/Customers/Journals/roam/20221108070724-service_cases.org")
(setq +service-case-template "/mnt/c/Users/E341194/OneDrive - Honeywell/Customers/Journals/roam/CaseTemplate.org")

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
        (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")
        (sequency "CASE" "|" "CLOSED(C)" "REASSIGNED(R)")))

(set-email-account! "icloud"
  '((mu4e-sent-folder       . "/icloud/Sent Mail")
    (mu4e-drafts-folder     . "/icloud/Drafts")
    (mu4e-trash-folder      . "/icloud/Trash")
    (mu4e-refile-folder     . "/icloud/All Mail")
    (smtpmail-smtp-user     . "millejoh@mac.com")
    (mu4e-compose-signature . "---\nYours truly\nJohn"))
  t)

(setq mu4e-update-interval 60)

(setq switch-to-buffer-obey-display-actions t)

(conda-env-activate "base")
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
