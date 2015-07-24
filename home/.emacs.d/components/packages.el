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
      '(adoc-mode yasnippet company yaml-mode markdown-mode org rbenv alchemist ido-vertical-mode color-theme-solarized coffee-mode rspec-mode bundler robe magit projectile projectile-rails flx-ido))

;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
