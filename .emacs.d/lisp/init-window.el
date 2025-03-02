;;; init-window.el -- configuration of window commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package window
  :bind
  ;; Bind `M-o' (by default bound to a rarely used command) to `other-window'.
  ("M-o" . other-window)

  ;; Bind `C-x M-k' to bury the current buffer, a command that's not bound to
  ;; any key by default.
  ("C-x M-k". bury-buffer))

(provide 'init-window)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-window.el ends here
