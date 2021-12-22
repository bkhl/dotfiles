;; The main Emacs configuration file
;;
;; This has only sets up essential configuration for the package system and
;; org-mode, then loads the configuration.org file.

;; Increase number of bytes of consing between garbage collections. This appears
;; to increase performance at the cost of some memory increase. This is done
;; first to decrease start-up time.
(setq gc-cons-threshold 20000000)

;; During loading of init file, disable checking filenames against the list of
;; filetype handlers. This speeds up startup, as otherwise this list would be
;; checked for every loaded .el and .elc file.
(let ((file-name-handler-alist nil))
  ;; Load early-init.el if we are on an Emacs version that didn't already load it.
  (when (version< emacs-version "27")
    (load-file (concat user-emacs-directory "early-init.el")))

  ;; Save custom values in separate file.
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file :noerror)

  ;; Work around HTTPS issues on older versions of Emacs.
  (when (version< emacs-version "26.3")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

  ;; Add load path for custom packages
  (let ((default-directory  "~/.emacs.d/site-lisp/"))
    (setq load-path
          (append
           (let ((load-path  (copy-sequence load-path)))
             (normal-top-level-add-subdirs-to-load-path))
           load-path)))

  ;; Load use-package.
  (require 'use-package)

  ;; Load org-mode.
  (use-package org)

  ;; Load the remainder of the configuration from an org-mode file.
  (org-babel-load-file (concat user-emacs-directory "configuration.org")))
