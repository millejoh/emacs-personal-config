;;; work-config.el -*- lexical-binding: t; -*-

(setq org-directory "/mnt/c/Users/E341194/OneDrive - Honeywell/Customers/Journals/")
(setq org-roam-directory "/mnt/c/Users/E341194/OneDrive - Honeywell/Customers/Journals/roam/")
(setq org-roam-dailies-directory "/mnt/c/Users/E341194/OneDrive - Honeywell/Customers/Journals/roam/daily/")
(setq org-agenda-files '("/mnt/c/Users/E341194/OneDrive - Honeywell/Customers/Journals/roam" "/mnt/c/Users/E341194/OneDrive - Honeywell/Customers/Journals/roam/daily"))
(setq +service-forecast-file "/mnt/c/Users/E341194/OneDrive - Honeywell/Customers/Journals/roam/20220622102420-service_forecast.org")
(setq +service-forecast-template "/mnt/c/Users/E341194/OneDrive - Honeywell/Customers/Journals/roam/ForecastTemplate.org")
(setq +service-case-file "/mnt/c/Users/E341194/OneDrive - Honeywell/Customers/Journals/roam/20221108070724-service_cases.org")
(setq +service-case-template "/mnt/c/Users/E341194/OneDrive - Honeywell/Customers/Journals/roam/CaseTemplate.org")
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "wslview")

(setq org-file-apps '((remote . emacs)
                      (auto-mode . emacs)
                      (directory . emacs)
                      ("\\.mm\\'" . "wslview \"%s\"")
                      ("\\.x?html?\\'" . "wslview \"%s\"")
                      ("\\.pptx?\\'" . "wslview \"%s\"")
                      ("\\.xlsx?\\'" . "wslview \"%s\"")
                      ("\\.docx?\\'" . "wslview \"%s\"")
                      ("\\.pdf\\'" .  "wslview \"%s\"")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
        (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")
        (sequency "CASE" "|" "CLOSED(C)" "REASSIGNED(R)")))

;;(defalias 'mu4e-search #'mu4e-headers-search)
;; (add-load-path! "/usr/local/share/emacs/site-lisp/mu4e/")

;; (set-email-account! "icloud"
;;   '((mu4e-sent-folder       . "/icloud/Sent Mail")
;;     (mu4e-drafts-folder     . "/icloud/Drafts")
;;     (mu4e-trash-folder      . "/icloud/Tr ash")
;;     (mu4e-refile-folder     . "/icloud/All Mail")
;;     (smtpmail-smtp-user     . "millejoh@mac.com")
;;     (mu4e-compose-signature . "---\nYours truly\nJohn"))
;;   t)

;; (setq mu4e-update-interval 60)
