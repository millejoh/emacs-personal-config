;; -*- mode: emacs-lisp-mode; lexical-binding: t -*-

;; Imenu based folding for origami, per issue #[69](https://github.com/gregsexton/origami.el/issues/69)

(defun origami-parser-imenu-flat (create)
  "Origami parser producing folds for each imenu entry, without nesting."
  (require 'imenu)
  (lambda (content)
    (let ((orig-major-mode major-mode))
      (with-temp-buffer
        (insert content)
        (funcall orig-major-mode)
        (let* ((items
                (-as-> (imenu--make-index-alist t) items
                       (-flatten items)
                       (-filter 'listp items)))
               (positions
                (-as-> (-map #'cdr items) positions
                       (-filter 'identity positions)
                       (-map-when 'markerp 'marker-position positions)
                       (-filter 'natnump positions)
                       (cons (point-min) positions)
                       (-snoc positions (point-max))
                       (-sort '< positions)
                       (-uniq positions)))
               (ranges
                (-zip-pair positions (-map '1- (cdr positions))))
               (fold-nodes
                (--map
                 (-let*
                     (((range-beg . range-end) it)
                      (line-beg
                       (progn (goto-char range-beg)
                              (line-beginning-position)))
                      (offset
                       (- (min (line-end-position) range-end) line-beg))
                      (fold-node
                       (funcall create line-beg range-end offset nil)))
                   fold-node)
                 ranges)))
          fold-nodes)))))

(provide 'imenu-origami)
