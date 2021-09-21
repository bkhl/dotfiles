;; Disable tool bars and scroll bar. Doing this here in early-init.el means they
;; won't appear during the startup process.
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Recommended when using straight.el
(when (version< "26" emacs-version)
  (setq package-enable-at-startup nil))
