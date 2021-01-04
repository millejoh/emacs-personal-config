
;;;
;;; org-config.el --- Personal setup for org
;;;
;;; Author: John M. Miller <millejoh@mac.com>
;;;


(require 'ob-ein)

(use-package ox-pandoc)

(org-babel-do-load-languages
 'org-babel-load-languages
 `((emacs-lisp . t)
   (lisp . t)
   (python . t)
   (ein . t)))

;; For the work windows machine


