;;; init-files.el -- configuration of file input and output commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package files
  :custom
  ;; Prompt before closing Emacs.
  (confirm-kill-emacs 'y-or-n-p))

(provide 'init-files)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-files.el ends here
