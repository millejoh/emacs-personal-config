
;;;
;;; org-config.el --- Personal setup for org
;;;
;;; Author: John M. Miller <millejoh@mac.com>
;;;

(use-package org
  :straight org ;; org-plus-contrib
  :config
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))


(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-db-autosync-mode)) 

(use-package ox-pandoc)

(use-package ox-rst)

(use-package ox-reveal
  :config
  (setq org-reveal-root "file:///c:/Users/E341194/msys2/home/E341194/code/reveal.js"))

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


