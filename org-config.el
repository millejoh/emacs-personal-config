
;;;
;;; org-config.el --- Personal setup for org
;;;
;;; Author: John M. Miller <millejoh@mac.com>
;;;



(use-package org
  :straight org-plus-contrib
  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))


(use-package ox-pandoc)

(use-package evil-org)

(require 'ob-ein)

(require 'org-protocol)

(org-babel-do-load-languages
 'org-babel-load-languages
 `((emacs-lisp . t)
   (lisp . t)
   (python . t)
   (ein . t)))

;; For the work windows machine


