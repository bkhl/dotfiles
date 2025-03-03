;;; init.el -*- no-byte-compile: t -*-

(defun my/load-configuration ()
  "Load configuration from configuration.org"

  (interactive)

  ;; During loading of init file, disable checking filenames against the list of
  ;; filetype handlers. This speeds up startup, as otherwise this list would be
  ;; checked for every loaded .el and .elc file.
  (let ((file-name-handler-alist nil)
        (gc-cons-threshold most-positive-fixnum))
    (org-babel-load-file (concat user-emacs-directory "configuration.org"))))

(my/load-configuration)

;; Set `gc-cons-threshold' to value used during normal operation, after
;; increasing it in `early-init.el'.
(setq gc-cons-threshold (eval-when-compile (* 32 1024 1024)))
