;;; init-paren.el -- configuration of matching parenthesis highlighting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package paren
  :custom
  ;; Show matching parenthesis context when offscreen
  (setopt show-paren-context-when-offscreen 'overlay))

(provide 'init-paren)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-paren.el ends here
