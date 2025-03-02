;;; init-startup.el -- configuration of startup -*- lexical-binding: t -*-
;;; Commentary:

;; This is configuration of variables controlling `startup.el', which is not a
;; regular Emacs package, so no `use-package' here.

;;; Code:

;; Show init time on startup
(advice-add 'display-startup-echo-area-message
            :after
            (defun my/display-startup-echo-area-message ()
              (message "Emacs init time: %s" (emacs-init-time))))

(provide 'init-startup)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-startup.el ends here
