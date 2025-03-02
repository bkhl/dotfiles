;;; init-use-package.el -- use-package configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; The `use-package' macro makes it easier to configure add-on packages without
;; generally having to remember how to set up autoloading &c.
;;
;; The configuration contains extensions to this macro to enable using packages from
;; Git submodules without involving a package manager.

(require 'use-package)

;; This makes the `:load-path' keyword default to a directory under
;; '/.emacs.d/site-lisp/' named the same as the package to be imported, if such
;; a directory exists. For packages where for some reason the package name
;; doesn't match the directory name, or that has its Lisp files in a
;; subdirectory, an explicit `:load-path' needs to be added.
(setf (alist-get :load-path use-package-defaults)
      '((lambda (name args)
          (let ((path (expand-file-name (symbol-name name)
                                        (expand-file-name "site-lisp"
                                                          user-emacs-directory))))
            (when (file-exists-p path)
              (list path))))
        t))

;; Some packages rely on `package.el' (or equivalent) to generate and register
;; an autoloads file. This custom keyword will do that for packages included
;; without a package manager.
;;
;; This is the function that actually generates the autoload file (if needed)
;; and loads it.
;;
;; Since `make-directory-autoloads' has no logic to work out if the file needs
;; to be regenerated, this function will skip calling it the file exists
;; already, and is newer than all the other Elisp files in the directory.
;;
;; If a package has additional files in subdirectories and such, this function
;; would not handle it, but then neither would `package-generate-autoloads', as
;; far as I can tell, so this should work for any packages supported by
;; `package.el'.
(defun my/use-package-autoload-package (name package-directory)
  "Set up autoloading for package NAME in directory PACKAGE-DIRECTORY."
  (let* ((name (symbol-name name))
         (auto-file (expand-file-name (format "%s/%s-autoloads.el"
                                              package-directory
                                              name))))
    (when (or (not (file-exists-p auto-file))
              (let* ((autoloads-attributes
                      (file-attributes auto-file))
                     (autoloads-age
                      (file-attribute-modification-time
                       autoloads-attributes))
                     (autoloads-inode-number
                      (file-attribute-inode-number autoloads-attributes)))
                (seq-find (lambda (attributes)
                            (time-less-p autoloads-age
                                         (file-attribute-modification-time
                                          attributes)))
                          (mapcar #'cdr
                                  (directory-files-and-attributes
                                   package-directory
                                   nil
                                   (rx ".el" eos))))))
      (make-directory-autoloads package-directory auto-file))
    (load auto-file package-directory)))

;; Register the custom keyword. This is added first in the list, so that it will
;; have access to the `:load-path' parameter, and so that it will load before
;; `:defer' or other keywords that might cause this to run after the package is
;; loaded.
(add-to-list 'use-package-keywords :make-autoloads)

;; This makes the keyword take boolean parameters similar to other keywords like
;; `:defer'.
(defalias 'use-package-normalize/:make-autoloads
  'use-package-normalize-predicate)

;; The handler function is what injects the call to the function to generate the
;; autoloads file when the `use-package' macro is expanded.
(defun use-package-handler/:make-autoloads (name _keyword arg rest state)
  (use-package-concat
     (mapcar #'(lambda (path)
                 `(my/use-package-autoload-package ',name ,path))
             (plist-get rest :load-path))
     (use-package-process-keywords name rest state)))

(provide 'init-use-package)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-use-package.el ends here
