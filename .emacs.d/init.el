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

  ;; Save custom values in separate file.
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file :noerror)

  ;; Set up package.el.
  (require 'package)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")))

  ;; Ensure that use-package is installed.
  (when (not (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'use-package))

  ;; Make use-package error if a declared package is missing.
  (setq use-package-always-ensure t)

  ;; Enable auto-compile.
  (use-package auto-compile
    :config
    (auto-compile-on-load-mode)
    (setq load-prefer-newer t))

  ;; Load org-mode.
  (use-package org)

  ;; Confirm that org-mode has been updated. This is a lazy solution until I can
  ;; work out how to check what the most recent version in the org-mode ELPA
  ;; repository is.
  (when (version-list-<= (mapcar (lambda (s) (string-to-number s))
                                 (split-string org-version "\\."))
                         '(9 3))
    (error "Installed version of org too old. Update manually using list-packages."))

  (org-babel-load-file (concat user-emacs-directory "configuration.org")))