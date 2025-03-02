;;; init-keymap.el -- configuration of keymaps -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package keymap
  :config
  ;; Disabling `C-z', which normally minimizes the window, which is rather
  ;; distracting.
  (keymap-global-unset "C-z"))

(provide 'init-keymap)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-keymap.el ends here
