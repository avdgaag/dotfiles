(use-package enh-ruby-mode
  :commands enh-ruby-mode
  :ensure t
  :interpreter "ruby"
  :mode ("\\.rb$"
         "\\.rake$"
         "Rakefile$"
         "\\.gemspec$"
         "\\.ru$"
         "Gemfile$"
         "Vagrantfile$"
         ".simplecov$")
  :init
  (setq ruby-insert-encoding-magic-comment nil)
  (setq enh-ruby-add-encoding-comment-on-save nil)
  (setq enh-ruby-deep-indent-paren nil)
  (setq enh-ruby-hanging-paren-deep-indent-level 2))

(use-package projectile-rails
  :commands projectile-rails-on
  :ensure t
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package rbenv
  :commands global-rbenv-mode
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'global-rbenv-mode)
  (add-hook 'enh-ruby-mode-hook 'rbenv-use-corresponding))

(use-package robe
  :commands robe-mode
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'robe-mode))

(use-package yard-mode
  :commands yard-mode
  :ensure t
  :init
  (add-hook 'enh-ruby-mode-hook 'yard-mode))

(use-package rspec-mode
  :commands rspec-mode
  :ensure t
  :init
  (setq rspec-use-bundler-when-possible nil)
  (setq rspec-use-rake-when-possible nil))

(use-package bundler
  :ensure t)

(use-package inf-ruby
  :ensure t)

(use-package ruby-refactor
  :ensure t)

(use-package rubocop
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package haml-mode
  :commands haml-mode
  :ensure t)

(use-package slim-mode
  :commands slim-mode
  :ensure t)

(use-package minitest
  :ensure t)

(load-library "~/.emacs.d/rails-sql-mode")
(add-hook 'projectile-rails-mode 'rails-sql-mode)
