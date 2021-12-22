;; The main Emacs configuration file
;;
;; Loads the configuration.org file after some performance tuning.

;; Increase number of bytes of consing between garbage collections. This appears
;; to increase performance at the cost of some memory increase. This is done
;; first to decrease start-up time.
(setq gc-cons-threshold 20000000)

;; During loading of init file, disable checking filenames against the list of
;; filetype handlers. This speeds up startup, as otherwise this list would be
;; checked for every loaded .el and .elc file.
(let ((file-name-handler-alist nil))
  ;; Load the remainder of the configuration from the Org configuration file.
  (org-babel-load-file (concat user-emacs-directory "configuration.org")))
