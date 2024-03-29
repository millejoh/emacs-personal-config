;;;
;;; init.el --- My bespoke emacs environment.
;;;
;;; Author: John M. Miller <millejoh@mac.com>
;;;

(defvar *my-config-directory* "~/custom-emacs/")

(setq ring-bell-function 'ignore)
(setq visual-bell t)

(show-paren-mode 1)
(defvar comp-deferred-compilation-deny-list nil)
(setq inhibit-compacting-font-caches t)
(when (boundp 'w32-pipe-read-delay)
  (setq 32-pipe-read-delay 0))

;; Package management by straight.el

(defvar bootstrap-version)
(defvar straight-use-package-by-default t)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(require 'use-package)

(use-package edit-server
  :config
  (edit-server-start))

;; Simulate portacle if it isn't present

(defun config-path (path)
  (if (locate-library "portacle")
      (portacle-path (concat "config/config.d/" path))
    (concat *my-config-directory* path)))


(if (locate-library "portacle")
    (config-path "portacle-support.el"))

;; Customizations (make this machine dependent)

(setq custom-file (config-path "customizations.el"))
(load custom-file)

;; Essentials: Evil / Which-key / General / avy / ivy
(use-package s)
(use-package yaml-mode)

(load (config-path "evil-config.el"))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  :commands (ivy-switch-buffer))

(use-package counsel
  :commands (counsel-M-x))

(use-package swiper)

(use-package avy
  :commands (avy-goto-word-1))

(use-package general
  :config
  (general-auto-unbind-keys)
  (general-define-key :states '(normal visual insert emacs)
		      :keymaps 'override
                      :prefix "SPC"
                      :non-normal-prefix "C-SPC"
                      
                      "SPC" '(counsel-M-x :which-key "M-x")
                      
                      ;; Buffer Commands
                      "b" '(:ignore t :which-key "buffers")
                      "bb" 'ivy-switch-buffer
                      "bI" 'ibuffer
                      "bd" 'kill-current-buffer

                      ;; File Commands
                      "fs" 'save-buffer
                      "ff" 'find-file

                      ;; Window Commands
                      "w" '(:ignore t :which-key "windows")
                      "wh" 'evil-window-left
                      "wl" 'evil-window-right
                      "wk" 'evil-window-up
                      "wj" 'evil-window-down
                      "w/" 'split-window-horizontally

                      ;; Frames
                      "F" '(:ignore t :which-key "Frames")
                      "F0" 'delete-frame
                      "F1" 'delete-other-frames
                      "F2" 'make-frame-command
                      "Fb" 'switch-to-buffer-other-frame
                      "Ff" 'find-file-other-frame
                      "Fd" 'dired-other-frame

                      ;; Toggles
                      "T" '(:ignore t :which-key "UI Toggles/Themes")
                      "TF" 'toggle-frame-fullscreen
                      "Tm" 'menu-bar-mode

                      ;; Jumps
                      "j" '(:ignore t :which-key "jump/join/split")
                      "jl" 'avy-goto-line
                      "jw" 'avy-goto-word-1
                      "jc" 'avy-goto-char

                      ;; Exiting Emacs

                      "q" '(:ignore t :which-key "Exit")
                      "qq" 'save-buffers-kill-terminal
                      "qQ" 'kill-emacs

                      ;; Applications
                      "a" '(:ignore t :which-key "Applications")
                      "ar" 'ranger
                      "ad" 'dired
                      "ay" '(:ignore t :which-key "Emacs IPython Notebook")
                      "ayr" 'ein:run
                      "ayl" 'ein:login
                      "ayd" 'ein:stop

                      ;; Magit
                      "g" '(:ignore t :which-key "Magit")
                      "gs" 'magit-status
                      "gc" 'magit-clone

		      ;; org-roam
		      "n" '(:ignore t :which-key "Notes")
		      "nr" '(:ignore t :which-key "Roam")
		      "nra" 'org-roam-alias-add
		      "nri" 'org-roam-node-insert
		      "nrf" 'org-roam-node-find
		      "nrc" 'org-roam-capture
		      "nrC" 'org-roam-dailies-capture-today
		      "nrb" 'org-roam-buffer-toggle
		      "nrB" 'org-roam-buffer-display-dedicated
		      "nrd" '(:ignore t :which-key "Dailies")
		      "nrdt" 'org-roam-dailies-goto-today
		      "nrdd" 'org-roam-dailies-goto-date
		      "nrdm" 'org-roam-dailies-goto-tomorrow
		      "nrdy" 'org-roam-dailies-goto-yesterday

		      ;;
		      "H h"  '(i-ching-insert-hexagram)))

(use-package which-key
  :init
  (setq which-key-separator " ")

  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1)
  (which-key-setup-minibuffer)) ;; This looks better in nano

(use-package load-env-vars)

(let ((env-vars-file (config-path "work.env")))
  (when (and (eql system-type 'windows-nt) (file-exists-p env-vars-file))
    (load-env-vars env-vars-file))) ;; Need to detect which windows host?

;; Move to somewhere else?
(use-package visual-fill-column
  :config (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package magit-section)

(use-package magit)

(use-package forge
  :after magit)

(use-package ibuffer-projectile
    :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package pdf-tools
  :straight (pdf-tools :type git :host github :repo "vedang/pdf-tools"))

(use-package elpy
  :ensure t
  :init (elpy-enable))

(use-package markdown-mode)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package pandoc-mode)

(use-package page-break-lines
  :config
  (global-page-break-lines-mode 1))

(use-package ranger)

(use-package dired-sort-menu)
(use-package dired-sort-menu-plus
  :defer t
  :straight (dired-sort-menu-plus :type git :host github :repo "emacsmirror/dired-sort-menu-plus"))

(require 'dired-sort-menu+)
(load (config-path "info+.el"))

;; Yasnippets

(use-package yasnippet)

(use-package yasnippet-snippets
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode #'yas-minor-mode)
  (add-hook 'writer-mode #'yas-minor-mode))

;; Configuring the rest

(load (config-path "user.el"))
(load (config-path "common-lisp.el"))
(load (config-path "ein-config.el"))
(load (config-path "org-config.el"))
(load (config-path "elisp-config.el"))

;; Fix company annoyance
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)

;; Nano
(use-package mini-frame)

(use-package nano-theme
  :straight (nano-theme :type git :host github
                                   :repo "rougier/nano-theme"))

(use-package nano-emacs
  :defer t
  :straight (nano-emacs :type git :host github :repo "rougier/nano-emacs")
  :config
  (setq nano-font-family-monospaced "Roboto Mono")
  (setq nano-font-size 12)
  :init
  (require 'nano-layout)
  (require 'nano-faces)
  (require 'nano-theme-dark)
  (require 'nano-theme-light)
  (require 'nano-theme)
  (nano-dark)
  (require 'nano-modeline)
  ;; (require 'nano-compact)
  )		


