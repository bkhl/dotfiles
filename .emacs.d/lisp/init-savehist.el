;;; init-savehist.el -- configuration of package to save minibuffer history -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package savehist
  :config
  ;; Preserve ~M-x~ command history between sessions.
  (savehist-mode))

(provide 'init-savehist)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-savehist.el ends here
