;;; Making editing Elisp more like using Slime, from Hemlut Eller ;;;

(load "~/.emacs.d/list-callers.el")

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
    ((kbd "C-c .")     'elisp-find-definition)
    ((kbd "C-c ,")     'elisp-pop-found-function)
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
