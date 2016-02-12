;;; ruby.el --- configure Ruby and related packages

;;; Commentary:

;;; Code:

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '(".simplecov$" . enh-ruby-mode))

;; For Ruby files, enable projectile and projectile-rails
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(add-hook 'enh-ruby-mode-hook 'global-rbenv-mode)
(add-hook 'enh-ruby-mode-hook 'rbenv-use-corresponding)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'yard-mode)

(load-library "~/.emacs.d/rails-sql-mode")
(add-hook 'projectile-rails-mode 'rails-sql-mode)

;; Prevent ruby-mode from adding magic encoding comments to the top of files
(setq ruby-insert-encoding-magic-comment nil)
(setq enh-ruby-add-encoding-comment-on-save nil)
(setq enh-ruby-deep-indent-paren nil)
(setq enh-ruby-hanging-paren-deep-indent-level 2)

(setq rspec-use-bundler-when-possible nil)
(setq rspec-use-rake-when-possible nil)

;;; ruby.el ends here
