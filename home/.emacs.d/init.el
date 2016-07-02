;;; init.el --- Arjan van der Gaag's Emacs configuration

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Main configuration file for custom settings.

;;; License:

;; Whatever.

;;; Code:

(setq load-prefer-newer t)

;; Define a location for custom configuration components
(setq avdg-emacs-components-dir
      (expand-file-name "components" user-emacs-directory))
(setq avdg-emacs-themes-dir
      (expand-file-name "themes" user-emacs-directory))
(setq avdg-emacs-core-dir
      (expand-file-name "core" user-emacs-directory))

(add-to-list 'load-path avdg-emacs-core-dir)
(add-to-list 'load-path avdg-emacs-components-dir)
(add-to-list 'custom-theme-load-path avdg-emacs-themes-dir)

(require 'ag-editor)
(require 'ag-functions)
(require 'ag-keys)
(require 'ag-osx)

(require 'ag-packages)
(require 'ag-exec-path-from-shell)
(require 'ag-ag)
(require 'ag-asciidoc)
(require 'ag-avy)
(require 'ag-clojure)
(require 'ag-company)
(require 'ag-css)
(require 'ag-elixir)
(require 'ag-expand-region)
(require 'ag-flycheck)
(require 'ag-helm)
(require 'ag-javascript)
(require 'ag-magit)
(require 'ag-markdown)
(require 'ag-multiple-cursors)
(require 'ag-org)
(require 'ag-paredit)
(require 'ag-projectile)

(require 'ag-ruby)
(require 'ag-sh)
(require 'ag-ui)
(require 'ag-web_mode)
(require 'ag-whitespace)
(require 'ag-yas)
(require 'ag-zencoding)
(require 'ag-elm)
(require 'ag-mu4e)
(require 'ag-diminish)
(require 'projectile-hanami)

(require 'diminish)
(diminish 'subword-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "f4020e085253f630b9dedb0bb2bea7dc574100b7993cac011f94097c4f92fd13" default))))

;; Use the Smyx color scheme
(load-theme 'smyx)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "Source Code Pro for Powerline")))))

;;; init.el ends here
