;;; init-custom.el -- configuration for customize -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package cus-edit
  :custom
  ;; This makes the Emacs customization interface store values in a separate file,
  ;; instead of in `init.el'.
  (custom-file (concat user-emacs-directory "custom.el"))
  :config
  ;; Load `custom-file' on startup.
  (load custom-file :noerror))

(provide 'init-custom)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-custom.el ends here
