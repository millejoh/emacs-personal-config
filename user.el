;;; Configure smartparens

;; (when (require 'paredit nil 'noerror)
;;   (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;;   (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;;   (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;   (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;;   (add-hook 'scheme-mode-hook           #'enable-paredit-mode))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(straight-use-package
 '(shrface :type git :repo "chenyanming/shrface" :host github))

(with-eval-after-load 'shr ; lazy load is very important, it can save you a lot of boot up time
  (require 'shrface)
  (shrface-basic) ; enable shrfaces, must be called before loading eww/dash-docs/nov.el
  (shrface-trial) ; enable shrface experimental face(s), must be called before loading eww/dash-docs/nov.el
  (setq shrface-href-versatile t) ; enable versatile URL faces support
                                  ; (http/https/ftp/file/mailto/other), if
                                  ; `shrface-href-versatile' is nil, default
                                  ; face `shrface-href-face' would be used.
  ;; eww support
  (with-eval-after-load 'eww
    (add-hook 'eww-after-render-hook 'shrface-mode)
    (evil-define-key '(evilified normal) eww-mode-map
      (kbd "<tab>") 'org-cycle
      (kbd "<S-tab>") 'org-shifttab
      (kbd "C-j") 'outline-next-visible-heading
      (kbd "C-k") 'outline-previous-visible-heading) )

  ;; nov support
  (with-eval-after-load 'nov
    (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title))) ; reset nov-shr-rendering-functions, in case of the list get bigger and bigger
    (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
    (add-hook 'nov-mode-hook 'shrface-mode)
    (evil-define-key '(evilified normal) nov-mode-map
      (kbd "<tab>") 'org-cycle
      (kbd "<S-tab>") 'org-shifttab
      (kbd "C-j") 'outline-next-visible-heading
      (kbd "C-k") 'outline-previous-visible-heading) )

  ;; mu4e support
  (with-eval-after-load 'mu4e
    (add-hook 'mu4e-view-mode-hook 'shrface-mode)))


;;; Useful commands for windows
(defun copy-for-ccm (start end)
  (interactive
   (list (region-beginning) (region-end)))
  (save-excursion
    (unfill-paragraph-region start end)
    (clipboard-kill-ring-save start end)
    (fill-region start end)))

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-paragraph-region (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (and (< (point) end) (not (eobp)))
      (beginning-of-line)
      (unfill-paragraph)
      (forward-line 2))))

(defun unfill-paragraph-buffer ()
  (interactive)
  (save-excursion
    (while (not (eobp))
      (beginning-of-line)
      (unfill-paragraph)
      (forward-line 2))))

(defun remove-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun load-directory (dir)
  (let ((load-it (lambda (f)
		               (load-file (concat (file-name-as-directory dir) f)))))
	  (mapc load-it (directory-files dir nil "\\.el$"))))

;;; Get rid of the bell
(defun my-mode-line-visual-bell ()
  (setq visible-bell nil)
  (setq ring-bell-function 'my-mode-line-visual-bell--flash))

(defun my-mode-line-visual-bell--flash ()
  (let ((frame (selected-frame)))
    (run-with-timer
     0.1 nil
     #'(lambda (frame)
         (let ((inhibit-quit)
               (inhibit-redisplay t))
           (invert-face 'header-line frame)
           (invert-face 'header-line-highlight frame)
           (invert-face 'mode-line frame)
           (invert-face 'mode-line-inactive frame)))
     frame)
    (let ((inhibit-quit)
          (inhibit-redisplay t))
      (invert-face 'header-line frame)
      (invert-face 'header-line-highlight frame)
      (invert-face 'mode-line frame)
      (invert-face 'mode-line-inactive frame))))

(my-mode-line-visual-bell)

;;; ------------------------------------------------------------ ;;;

;;; Making editing Elisp more like using Slime, from Hemlut Eller ;;;

;(require 'list-callers)

(defun elisp-disassemble (function)
  (interactive (list (function-called-at-point)))
  (disassemble function))

(defun elisp-pp (sexp)
  (with-output-to-temp-buffer "*Pp Eval Output*"
    (pp sexp)
    (with-current-buffer standard-output
      (emacs-lisp-mode))))

(defun elisp-macroexpand (form)
  (interactive (list (form-at-point 'sexp)))
  (elisp-pp (macroexpand form)))

(defun elisp-macroexpand-all (form)
  (interactive (list (form-at-point 'sexp)))
  (elisp-pp (cl-macroexpand-all form)))

(defun elisp-push-point-marker ()
  (require 'etags)
  (cond ((featurep 'xemacs)
         (push-tag-mark))
        (t (ring-insert find-tag-marker-ring (point-marker)))))

(defun elisp-pop-found-function ()
  (interactive)
  (cond ((featurep 'xemacs) (pop-tag-mark nil))
        (t (pop-tag-mark))))

(defun elisp-find-definition (name)
  "Jump to the definition of the function (or variable) at point."
  (interactive (list (thing-at-point 'symbol)))
  (unless name
    (error "No symbol at point"))
  (let* ((symbol (intern-soft name))
         (find (cond ((fboundp symbol) 'find-function-noselect)
                     ((boundp symbol) 'find-variable-noselect)
                     (t (error "Symbol not bound: %S" symbol))))
         (r (save-excursion (funcall find symbol)))
         (buffer (car r))
         (point (cdr r)))
    (cond (point
           (elisp-push-point-marker)
           (switch-to-buffer buffer)
           (goto-char point)
           (recenter 1))
          (t
           (error "Found no definition for %s in %s" name buffer)))))

(defun elisp-bytecompile-and-load ()
  (interactive)
  (or buffer-file-name
      (error "The buffer must be saved in a file first"))
  (require 'bytecomp)
  ;; Recompile if file or buffer has changed since last compilation.
  (when  (and (buffer-modified-p)
              (y-or-n-p (format "save buffer %s first? " (buffer-name))))
      (save-buffer))
  (let ((filename (expand-file-name buffer-file-name)))
    (with-temp-buffer
      (byte-compile-file filename t))))

(defvar elisp-extra-keys
  '(((kbd "C-c d")   'elisp-disassemble)
    ((kbd "C-c m")   'elisp-macroexpand)
    ((kbd "C-c M")   'elisp-macroexpand-all)
    ((kbd "C-c C-c") 'compile-defun)
    ((kbd "C-c C-k") 'elisp-bytecompile-and-load)
    ((kbd "C-c C-l") 'load-file)
    ((kbd "C-c p")   'pp-eval-last-sexp)
    ((kbd "M-.")     'elisp-find-definition)
    ((kbd "M-,")     'elisp-pop-found-function)
    ((kbd "C-c <")   'list-callers)))

(dolist (binding elisp-extra-keys)
  (let ((key (eval (car binding))) (val (eval (cadr binding))))
    (define-key emacs-lisp-mode-map key val)
    (define-key lisp-interaction-mode-map key val)))

;;; Custom indentation for cl-indent ;;;

(put '%stack-block 'common-lisp-indent-function '((&whole 4 &rest (&whole 1 1 2)) &body))
(put 'rlet 'common-lisp-indent-function '((&whole 4 &rest (&whole 1 1 2)) &body))
(put 'create-schema 'common-lisp-indent-function '(6 (&whole 2 $rest 1)))
(put 'define-method 'common-lisp-indent-function 'lisp-indent-defmethod)

;;; Fun stuff

(defun sutra-spinner ()
  (interactive)
  (case (1+ (random 4))
    (1 (message "Read sutra 1.%s" (1+ (random 51))))
    (2 (message "Read sutra 2.%s" (1+ (random 55))))
    (3 (message "Read sutra 3.%s" (1+ (random 56))))
    (4 (message "Read sutra 4.%s" (1+ (random 34))))))
