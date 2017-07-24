;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar ag-dir (file-name-directory load-file-name)
  "The root directory of the Emacs configuration files.")
(defvar ag-core-dir (expand-file-name "core" ag-dir)
  "The home directory of core settings files.")
(defvar ag-modules-dir (expand-file-name "modules" ag-dir)
  "The directory containing all additional modules.")
(defvar ag-modules-file (expand-file-name "ag-modules.el" ag-dir)
  "List of additional modules to be loaded on startup.")
(defvar ag-vendor-dir (expand-file-name "vendor" ag-dir)
  "The directory containing vendor modules that could be installed via a package manager.")

(add-to-list 'load-path ag-core-dir)
(add-to-list 'load-path ag-modules-dir)
(add-to-list 'load-path ag-vendor-dir)

(require 'ag-general)
(require 'ag-packages)
(require 'ag-editor)
(require 'ag-ui)
(require 'ag-functions)
(require 'ag-keys)
(when (eq system-type 'darwin)
  (require 'ag-osx))

(if (file-exists-p ag-modules-file)
    (load ag-modules-file)
  (message "Missing modules file %s" ag-modules-file))

;; Start Emacs server to make subsequent launches faster
(require 'server)
(unless (server-running-p)
  (server-start))

(add-to-list 'safe-local-variable-values
             '(flycheck-javascript-eslint-executable . "/Users/arjan/code/rabobank/assets/node_modules/.bin/eslint"))
(add-to-list 'safe-local-variable-values
             '(prettier-command . "/Users/arjan/code/rabobank/assets/node_modules/.bin/prettier"))
