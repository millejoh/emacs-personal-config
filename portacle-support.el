;;;
;;; portacle-support.el --- Recreate some of portacle's config when
;;; not in a portacle environment.
;;;
;;; Author: John M. Miller <millejoh@mac.com>
;;;

  (use-package ido
      :config
    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t))
  (use-package smex
      :config
    (smex-initialize))
  (use-package helpful)
  (electric-indent-mode 1)
  (semantic-mode 1)
  (delete-selection-mode 1)
  (setq enable-local-variables :all)
  (setq version-control t)
  (setq vc-follow-symlinks t)
