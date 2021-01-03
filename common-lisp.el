;;;
;;; common-lisp.el --- Configure an environment for working with common-lisp
;;;
;;; Author: John M. Miller <millejoh@mac.com>
;;;


(use-package sly
  :config
  (setq inferior-lisp-program "ros run")
  :general
  (:keymaps 'sly-inspector-mode-map
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
  (:keymaps 'sly-db-mode-map
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
            "5" 'sly-db-invoke-restart-5))
