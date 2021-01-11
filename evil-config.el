;;;
;;; evil-config.el --- Setting up my evil environment. Muhaha.
;;;
;;; Author: John M. Miller <millejoh@mac.com>
;;;

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1)
  (evil-define-key '(normal insert) 'global [?\t] 'indent-for-tab-command)
  (setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes)))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :config (evil-collection-init))
