;;; packages.el --- list and install Emacs packages

;;; Commentary:
;;;
;;; This file contains a list of all required emacs plugins and makes
;;; sure they are installed.

;;; Code:

(require 'package)

;; Add melpa as package repo
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
       '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Initialize package repo's
(package-initialize)

;; Refresh list of packages
(unless package-archive-contents
  (package-refresh-contents))

(setq package-list
      '(adoc-mode
        clojure-mode
        cider
        helm
        helm-projectile
        feature-mode
        paredit
        helm-ag
        avy
        php-mode
        minitest
        ack
        ag
        multiple-cursors
        yasnippet
        company
        yaml-mode
        haml-mode
        markdown-mode
        ruby-refactor
        org
        enh-ruby-mode
        inf-ruby
        rubocop
        yard-mode
        rbenv
        alchemist
        coffee-mode
        rspec-mode
        bundler
        robe
        magit
        js2-mode
        scss-mode
        sass-mode
        flycheck
        projectile
        projectile-rails
        slim-mode
        ggtags))

;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; packages.el ends here
