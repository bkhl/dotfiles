;;; init-server.el -- configuration of Emacs server -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package server
  :custom
  ;; Disable message on new `emacsclient' frames.
  (server-client-instructions nil))

(provide 'init-server)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-server.el ends here
