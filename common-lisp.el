;;;
;;; common-lisp.el --- Configure an environment for working with common-lisp
;;;
;;; Author: John M. Miller <millejoh@mac.com>
;;;

(unless (locate-library "portacle")
  (use-package sly
      :config
    (setq sly-auto-select-connection 'always)
    (setq sly-kill-without-query-p t)
    (setq sly-description-autofocus t) 
    (setq sly-inhibit-pipelining nil)
    (setq sly-load-failed-fasl 'always)
    (setq sly-ignore-protocol-mismatches t)
    (add-hook 'sly-mrepl-mode-hook 'electric-pair-local-mode)))

;; Paredit
(defun init-paredit ()
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(if (string= (system-name) "TX92LT1JW0YT2")
    (progn
      (load (config-path "paredit.el"))
      (init-paredit))
  (use-package paredit
    :init (init-paredit))) 
          

;; Define better keymaps
(general-define-key :keymaps 'sly-mode-map
                    :states 'normal
                    :prefix ","

                    "." 'sly-edit-definition
                    "," 'sly-pop-find-definition-stack
                    "?" 'sly-edit-uses
                    "dd" 'sly-disassemble-symbol
                    "dm" 'sly-macroexpand-all
                    "z" 'sly-mrepl
                    "E" 'sly-edit-value
                    "I" 'sly-inspect
                    "hf" 'sly-describe-function
                    "hs" 'sly-describe-symbol
                    "hp" 'sly-apropos-package
                    "hz" 'sly-apropos-all
                    "hH" 'common-lisp-hyperspec)

(general-define-key :keymaps 'sly-inspector-mode-map
                    :states 'normal
                    :prefix ","
                    "l" 'sly-inspector-pop
                    "n" 'sly-inspector-next
                    "SPC" 'sly-inspector-next
                    "D" 'sly-inspector-describe-inspectee
                    "e" 'sly-inspector-eval
                    "h" 'sly-inspector-history
                    "g" 'sly-inspector-reinspect
                    ">" 'sly-inspector-fetch-all
                    "q" 'sly-inspector-quit)

(general-define-key  :keymaps 'sly-db-mode-map
                     :states 'normal
                     :prefix ","
                     "n" 'sly-db-down
                     "p" 'sly-db-up
                     "J" 'sly-db-details-down
                     "K" 'sly-db-details-up
                     "<" 'sly-db-beginning-of-backtrace
                     ">" 'sly-db-end-of-backtrace
                     "a" 'sly-db-abort
                     "c" 'sly-db-continue
                     "A" 'sly-db-break-with-system-debugger
                     "B" 'sly-db-break-with-default-debugger
                     "P" 'sly-db-print-condition
                     "I" 'sly-db-invoke-restart-by-name
                     "C" 'sly-db-inspect-condition
                     ":" 'sly-interactive-eval
                     "q" 'sly-db-quit
                     "0" 'sly-db-invoke-restart-0
                     "1" 'sly-db-invoke-restart-1
                     "2" 'sly-db-invoke-restart-2
                     "3" 'sly-db-invoke-restart-3
                     "4" 'sly-db-invoke-restart-4
                     "5" 'sly-db-invoke-restart-5)
