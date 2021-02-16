;;;
;;; init.el --- My bespoke emacs environment.
;;;
;;; Author: John M. Miller <millejoh@mac.com>
;;;

(setq ring-bell-function 'ignore)
(setq visual-bell t)

(show-paren-mode 1)

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

;; Essentials: Evil / Which-key / General / avy / ivy
(use-package s)

(load (portacle-path "config/config.d/evil-config.el"))

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
                      "b" '(:ignore t :which-key "windows")
                      "bb" 'ivy-switch-buffer
                      "bI" 'ibuffer
                      "bd" 'kill-buffer

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

(let ((env-vars-file (portacle-path "config/config.d/work.env")))
  (when (and (eql system-type 'windows-nt) (file-exists-p env-vars-file))
    (load-env-vars env-vars-file))) ;; Need to detect which windows host?

;; Move to somewhere else?
;; (use-package magit)

(use-package pdf-tools)

;; (use-package company
;;   :config
;;   (add-hook 'after-init-hook 'global-company-mode))

(use-package pandoc-mode)

(use-package page-break-lines
  :config
  (global-page-break-lines-mode 1))

(load (portacle-path "config/config.d/info+.el"))

;; Configure iBuffer

(setq ibuffer-saved-filter-groups
      '(("default" 
	 ("magit-revision-mode"
	  (mode . magit-revision-mode))
	 ("markdown-mode"
	  (mode . markdown-mode))
	 ("python-mode"
	  (mode . python-mode))
	 ("org-mode"
	  (mode . org-mode))
	 ("text-mode"
	  (mode . text-mode))
	 ("ein:notebook"
	  (or
           (name . "\\*ein: http:.*\\*")
           (mode . ein:notebook)
           (predicate . ein:get-notebook--notebook)))
	 ("ein:notebooklist-mode"
	  (mode . ein:notebooklist-mode))
	 ("magit-status-mode"
	  (mode . magit-status-mode))
	 ("magit-log-mode"
	  (mode . magit-log-mode))
	 ("dired-mode"
	  (mode . dired-mode))
	 ("emacs-lisp-mode"
	  (mode . emacs-lisp-mode))
	 ("lisp"
	  (mode . lisp-mode))
	 ("ein:shared-output-mode"
	  (mode . ein:shared-output-mode)))))

(add-hook 'ibuffer-mode-hook
 	  '(lambda ()
 	     (ibuffer-switch-to-saved-filter-groups "default")))

;; Yasnippets

(use-package yasnippet)

(use-package yasnippet-snippets
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode #'yas-minor-mode)
  (add-hook 'writer-mode #'yas-minor-mode))

;; Paredit

;; (use-package paredit
;;   :init (progn
;;           (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;;           (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;;           (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;           (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;;           (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;;           (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;;           (add-hook 'scheme-mode-hook           #'enable-paredit-mode)))

;; Nano

(straight-use-package 
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(setq nano-use-light-theme t)

;; Configuring the rest

(load (portacle-path "config/config.d/user.el"))
(load (portacle-path "config/config.d/common-lisp.el"))
(load (portacle-path "config/config.d/ein-config.el"))
(load (portacle-path "config/config.d/org-config.el"))
(load (portacle-path "config/config.d/elisp-config.el"))
(load (portacle-path "config/config.d/nano.el"))

;; Customizations (make this machine dependent)

(setq custom-file (portacle-path "config/config.d/customizations.el"))
(load custom-file)
