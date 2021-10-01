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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; Other themes that seem to work:
;;  doom-flatwhite
;;  doom-palenite
;;  doom-plaindark
;;  doom-opera

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/Journal/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(use-package! avy
  :commands (avy-goto-word-1))

(use-package! ein
  :general
  (:states 'normal
	   :keymaps 'ein:notebook-mode-map
	   :prefix ","
           "RET" 'ein:worksheet-execute-cell-and-goto-next
	   "j" 'ein:worksheet-goto-next-input
           "k" 'ein:worksheet-goto-prev-input
           "J" 'ein:worksheet-move-cell-down
           "K" 'ein:worksheet-move-cell-up
           "e" 'ein:worksheet-toggle-output
           "d" 'ein:worksheet-kill-cell
           "y" 'ein:worksheet-copy-cell
           "p" 'ein:worksheet-yank-cell
           "m" 'ein:worksheet-merge-cell
           "s" 'ein:worksheet-split-cell-at-point
           "o" 'ein:worksheet-insert-cell-below
           "O" 'ein:worksheet-insert-cell-above
           "t" 'ein:worksheet-toggle-cell-type
           "u" 'ein:worksheet-toggle-cell-type ;; For historical reasons.
           "C-m" 'ein:worksheet-execute-cell
           "l" 'ein:worksheet-clear-output
           "L" 'ein:worksheet-clear-all-output
           "fs" 'ein:notebook-save-notebook-command
           "fc" 'ein:notebook-reconnect-session-command
           "fr" 'ein:notebook-restart-session-command
           "C-r" 'ein:notebook-rename-command
           "x" 'ein:notebook-close
           "gg" 'ein:kernel-utils-jump-to-source-command
           "hh" 'ein:kernel-utils-request-tooltip-or-help
           "z" 'ein:notebook-kernel-interrupt-command))


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

(org-babel-do-load-languages
 'org-babel-load-languages
 `((emacs-lisp . t)
   (lisp . t)
   (python .t)))

(after! ein
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((ein . t))))

(load "~/custom-emacs/.doom.d/elisp.el")

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
