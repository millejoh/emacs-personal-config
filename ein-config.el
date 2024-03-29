
;;;
;;; ein-config.el --- Personal setup for ein
;;;
;;; Author: John M. Miller <millejoh@mac.com>
;;;


(use-package ein
  :commands (ein:run ein:login)
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

(use-package pos-tip)

(use-package ein-kernel-utils
  :straight '(ein-kernel-utils :type git :host github :repo "millejoh/ein-kernel-utils")
  :config
  (require 'ein-kernel-completion)
  (require 'ein-buffer-connect)
  (add-hook 'ein:notebook-mode-hook #'(lambda () (add-to-list 'company-backends 'ein:company-backend)))
  (add-hook 'ein:notebook-mode-hook
            #'(lambda ()
                (when (featurep 'eldoc)
                  (add-function :before-until (local 'eldoc-documentation-function)
                                #'ein:completer-get-eldoc-signature)
                  (eldoc-mode))))
  (add-hook 'ein:on-kernel-connect-functions #'(lambda (kernel)
                                                 (ein:kernel-utils-load-safely kernel)))
  (add-hook 'org-src-mode-hook #'ein:on-edit-source-block)
  (add-hook 'hy-mode-hook
	    (lambda ()
	      (define-key python-mode-map "\C-c." 'ein:kernel-utils-jump-to-source)
	      (define-key python-mode-map "\C-c\C-h" 'ein:kernel-utils-request-tooltip-or-help)
	      (define-key python-mode-map "\C-c\C-c" 'ein:connect-run-or-eval-buffer)
	      (define-key python-mode-map "\C-c\C-l" 'ein:connect-reload-buffer)
	      (define-key python-mode-map "\C-c\C-r" 'ein:connect-eval-region)
	      (define-key python-mode-map (kbd "C-:") 'ein:shared-output-eval-string)
	      (define-key python-mode-map "\C-c\C-z" 'ein:connect-pop-to-notebook)
	      (define-key python-mode-map "\C-c\C-x" 'ein:tb-show)
	      (define-key python-mode-map (kbd "C-c C-/") 'ein:notebook-scratchsheet-open)))
  (add-hook 'python-mode-hook
            (lambda ()
              (define-key python-mode-map "\C-c." 'ein:kernel-utils-jump-to-source)
              (define-key python-mode-map "\C-c\C-h" 'ein:kernel-utils-request-tooltip-or-help)
              (define-key python-mode-map "\C-c\C-c" 'ein:connect-run-or-eval-buffer)
              (define-key python-mode-map "\C-c\C-l" 'ein:connect-reload-buffer)
              (define-key python-mode-map "\C-c\C-r" 'ein:connect-eval-region)
              (define-key python-mode-map (kbd "C-:") 'ein:shared-output-eval-string)
              (define-key python-mode-map "\C-c\C-z" 'ein:connect-pop-to-notebook)
              (define-key python-mode-map "\C-c\C-x" 'ein:tb-show)
              (define-key python-mode-map (kbd "C-c C-/") 'ein:notebook-scratchsheet-open))))
