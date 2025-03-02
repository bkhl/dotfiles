;;; init-saveplace.el -- configuration to automatically save place in files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package saveplace
  :config
  ;; Use saved point position in previously opened files.
  (save-place-mode))

(provide 'init-saveplace)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-saveplace.el ends here
