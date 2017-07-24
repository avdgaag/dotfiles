(require 'cl)
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(defvar ag-packages
  '(use-package
     diminish
     exec-path-from-shell)
  "List of packages that should always be installed.")

(defun ag-install-package (package)
  "Install PACKAGE if it is not yet installed."
  (unless (package-installed-p package)
    (package-install package)))

(defun ag-install-packages ()
  "Install all packages in `ag-packages`."
  (unless (every #'package-installed-p ag-packages)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" "Done.")
    (mapc #'ag-install-package ag-packages)))

(ag-install-packages)

(diminish 'whitespace-mode)

(provide 'ag-packages)
