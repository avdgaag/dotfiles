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
        ack
        multiple-cursors
        yasnippet
        company
        yaml-mode
        markdown-mode
        org
        enh-ruby-mode
        yard-mode
        rbenv
        alchemist
        ido-vertical-mode
        coffee-mode
        rspec-mode
        bundler
        robe
        magit
        js2-mode
        projectile
        projectile-rails
        flx-ido))

;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
