;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;;;
;;;  Configure org capture for common service actions
;;;
(load! "work-config.el")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Miller"
      user-mail-address "john.miller@uop.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 14)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 14))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'nano) ;; nano-dark

;; We like some whitespace between the fringe and text
(setq-default left-margin-width 1 right-margin-width 1)
;; (set-window-buffer nil (current-buffer))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")           ;

(setq inferior-lisp-program "ros run")

;; (setq conda-anaconda-home "/Users/E341194/anaconda3")

;; (conda-env-activate "base")

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

;; Because, in the end, paredit was all I ever needed. See
;; https://github.com/luxbock/evil-cleverparens for documentation.
;; (add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
;; (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)

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

(after! org
  (setq org-image-max-width 1200)
  (setq org-pretty-entities t)
  (setq org-auto-align-tags nil)
  (setq line-spacing 0.1)
  ;; (setq org-startup-indented nil)
  (add-to-list 'org-file-apps '("\\.docx$" . default)))

;;;
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

;;;; Better column display in org.
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


(setq switch-to-buffer-obey-display-actions t)
; (add-to-list 'Info-directory-list "~/.local/share/info")

;;; add to $DOOMDIR/config.el
(remove-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)


(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'text-mode-hook 'remove-dos-eol)

(defun unfill-paragraph (&optional region)
  "Make multi-line paragraph into a single line of text.

REGION unfills the region.  See URL
`https://www.emacswiki.org/emacs/UnfillParagraph'"
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; For better R support when editing org code blocks:
;;
(defvar org-ess-tmprs nil)
(defun get-org-current-rsession ()
  "When you are in an org file get the current R session based on global header, subtree, property or source code :session property whichever is relevant"
  (setq org-ess-tmprs (cdr (assoc :session (nth 2 (org-babel-get-src-block-info t))))))

(defun set-ess-R-process ()
  "set the process to session stored in tmprs variable"
  (message "Setting R session to %s" (eval org-ess-tmprs))
  (setq ess-local-process-name (process-name (get-buffer-process org-ess-tmprs))))


(advice-add 'org-edit-src-code :before #'get-org-current-rsession)
(advice-add 'org-edit-src-code :after #'set-ess-R-process)

;; Delete the following when Doom github issue #8449 is resolved.
;; Unclear, though, is PR #8434 will actually fix this.
;; (after! comint
;;   (advice-remove 'comint-output-filter #'doom--comint-enable-undo-a)
;;   (advice-remove 'comint-output-filter #'doom--comint-protect-output-in-visual-modes-a))

;;*
;;; add to $DOOMDIR/config.el

;; Remove then redefine the existing advice:
;; (advice-remove #'diff-hl-define-bitmaps #'+vc-gutter-define-thin-bitmaps-a)
;; (defadvice! +vc-gutter-define-thin-bitmaps-a (&rest _)
;;   :after #'diff-hl-define-bitmaps
;;   (let* ((scale (if (and (boundp 'text-scale-mode-amount)
;;                          (numberp text-scale-mode-amount))
;;                     (expt text-scale-mode-step text-scale-mode-amount)
;;                   1))
;;          (spacing (or (and (display-graphic-p) (default-value 'line-spacing)) 0))
;;          (h (+ (ceiling (* (frame-char-height) scale))
;;                (if (floatp spacing)
;;                    (truncate (* (frame-char-height) spacing))
;;                  spacing)))
;;          (w (min (frame-parameter nil (intern (format "%s-fringe" diff-hl-side)))
;;                  16))
;;          (_ (if (zerop w) (setq w 16))))
;;     (define-fringe-bitmap 'diff-hl-bmp-middle
;;       (make-vector
;;        h (string-to-number (concat (make-string (/ w 2) ?1)
;;                                    (make-string (- w (/ w 2)) ?0))
;;                            2))
;;       nil nil 'center)))
