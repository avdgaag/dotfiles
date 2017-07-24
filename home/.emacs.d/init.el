(defvar ag-dir (file-name-directory load-file-name)
  "The root directory of the Emacs configuration files.")
(defvar ag-core-dir (expand-file-name "core" ag-dir)
  "The home directory of core settings files.")
(defvar ag-modules-dir (expand-file-name "modules" ag-dir)
  "The directory containing all additional modules.")
(defvar ag-modules-file (expand-file-name "ag-modules.el" ag-dir)
  "List of additional modules to be loaded on startup.")

(add-to-list 'load-path ag-core-dir)
(add-to-list 'load-path ag-modules-dir)

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
