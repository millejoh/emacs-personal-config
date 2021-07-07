(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list '("/mingw64/share/"))
 '(browse-url-browser-function 'browse-url-default-windows-browser)
 '(case-fold-search t)
 '(company-minimum-prefix-length 2)
 '(company-occurrence-weight-function 'company-occurrence-prefer-any-closest)
 '(company-show-numbers 't)
 '(company-tng-mode nil)
 '(custom-safe-themes
   '("f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" default))
 '(edebug-print-length 200)
 '(ein:content-query-max-branch 2)
 '(ein:enable-eldoc-support t)
 '(ein:jupyter-default-notebook-directory "c:/Users/E341194/OneDrive - Honeywell/Customers")
 '(ein:jupyter-server-args '("--no-browser"))
 '(ein:output-area-inlined-images t)
 '(ein:query-timeout 100000)
 '(ein:slice-image t)
 '(evil-want-Y-yank-to-eol nil)
 '(fill-column 84)
 '(global-git-commit-mode t)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX" . "#dc752f")
     ("XXXX" . "#dc752f")))
 '(ibuffer-formats
   '((mark modified read-only locked " "
           (name 40 40 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))
 '(inferior-lisp-program "ros run")
 '(ispell-local-dictionary "en_US")
 '(ispell-program-name "c:/Users/E341194/msys2/mingw64/bin/aspell.exe")
 '(magit-git-executable "C:/Program Files/Git/cmd/git.exe")
 '(org-agenda-files
   "/Users/E341194/OneDrive - Honeywell/Customers/agenda_list.org")
 '(org-ascii-text-width 120)
 '(org-babel-lisp-eval-fn 'sly-eval)
 '(org-confirm-babel-evaluate nil)
 '(org-display-remote-inline-images 'cache)
 '(org-emphasis-alist
   '(("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t))))
 '(org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs)))
 '(org-list-allow-alphabetical t)
 '(org-num-max-level 4)
 '(org-pandoc-command "pandoc")
 '(org-preview-latex-process-alist
   '((dvipng :programs
             ("latex.exe" "dvipng.exe")
             :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
             (1.0 . 1.0)
             :latex-compiler
             ("latex -interaction nonstopmode -output-directory %o %f")
             :image-converter
             ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
     (dvisvgm :programs
              ("latex" "dvisvgm")
              :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :use-xcolor t :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
              (1.7 . 1.5)
              :latex-compiler
              ("latex -interaction nonstopmode -output-directory %o %f")
              :image-converter
              ("dvisvgm %f -n -b min -c %S -o %O"))
     (imagemagick :programs
                  ("latex" "convert")
                  :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("pdflatex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("convert -density %D -trim -antialias %f -quality 100 %O"))))
 '(paradox-github-token "3189073c51e5af6b4bfff52a68b1fa43477c7887")
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(python-indent-guess-indent-offset-verbose nil)
 '(request-curl-options nil)
 '(request-log-level -1)
 '(request-message-level -1)
 '(safe-local-variable-values
   '((Package . CLIM-INTERNALS)
     (Package . CLIM-CLX)
     (Syntax . COMMON-LISP)
     (Package . CL-USER)
     (Lowercase . Yes)
     (Package . COMMON-LISP-INTERNALS)
     (Syntax . Common-Lisp)
     (Base . 10)
     (Syntax . ANSI-Common-Lisp)
     (javascript-backend . tern)
     (javascript-backend . lsp)))
 '(user-full-name "John Miller")
 '(user-mail-address "millejoh@mac.com")
 '(warning-suppress-types '((ein)))
 '(with-editor-emacsclient-executable "emacsclient")
 '(with-editor-shell-command-use-emacsclient nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "outline" :family "Roboto Mono"))))
 '(face-strong ((t (:weight bold))) t)
 '(org-level-1 ((t (:inherit face-strong :extend nil :height 1.2))))
 '(org-level-2 ((t (:inherit face-strong :extend nil :height 1.1))))
 '(show-paren-match ((t (:inherit face-popout :background "dim gray"))))
 '(show-paren-match-expression ((t (:inherit show-paren-match :weight extra-bold :height 1.1)))))
