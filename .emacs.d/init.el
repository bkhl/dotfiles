;; During loading of init file, disable checking filenames against the list of
;; filetype handlers. This speeds up startup, as otherwise this list would be
;; checked for every loaded .el and .elc file.
(let ((file-name-handler-alist nil))
  ;; Load the configuration from the Org configuration file.
  (org-babel-load-file (concat user-emacs-directory "configuration.org")))
