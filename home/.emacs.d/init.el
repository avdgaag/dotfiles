;;; init.el --- Arjan van der Gaag's Emacs configuration

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Main configuration file for custom settings.

;;; License:

;; Whatever.

;;; Code:

;; Define a location for custom configuration components
(setq avdg-emacs-components-dir
      (expand-file-name "components" user-emacs-directory))

;; Load all files in the configuration components directory
(if (file-exists-p avdg-emacs-components-dir)
    (dolist (file
             (directory-files avdg-emacs-components-dir t "\\.el$"))
      (load file))
    (message "Configuration components directory not found; skipping."))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "f4020e085253f630b9dedb0bb2bea7dc574100b7993cac011f94097c4f92fd13" default)))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (require
            (quote projectile))
           (puthash
            (projectile-project-root)
            "bin/nanoc compile" projectile-compilation-cmd-map))
     (flycheck-eslint-executable . "/Users/arjan/code/vecozo/node_modules/eslint/bin/eslint.js")
     (flycheck-eslint-executable . /Users/arjan/code/vecozo/node_modules/eslint/bin/eslint\.js)
     (flycheck-disabled-checkers . javascript-jshint)
     (projectile-project-test-cmd . "mocha --no-colors --reporter dot"))))
 '(sql-product (quote postgres)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282828" :foreground "#F7F7F7" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Source Code Pro for Powerline")))))

;; Use the Smyx color scheme
(load-theme 'smyx)

;; Always autoscroll compilation output, so long reuslt listings are easier to
;; read.
(setq compilation-scroll-output 'first-error)

;; automatically tail opened log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

;;; init.el ends here
