(require 'use-package)

(use-package enh-ruby-mode
  :ensure t
  :interpreter "ruby"
  :mode ("\\.rb$"
         "\\.rake$"
         "Rakefile$"
         "\\.gemspec$"
         "\\.ru$"
         "Gemfile$"
         "Vagrantfile$"
         ".simplecov$"))

(use-package projectile-rails
  :ensure t
  :init
  (setq projectile-rails-add-keywords nil)
  (setq projectile-rails-expand-snippet nil)
  (setq projectile-rails-vanilla-command "bin/rails")
  (setq projectile-rails-spring-command "bin/spring")
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package bundler
  :ensure t
  :commands (bundle-open
             bundle-console
             bundle-install
             bundle-update
             bundle-check))

(use-package rubocop
  :ensure t
  :commands (rubocop-check-project
             rubocop-check-current-file
             rubocop-autocorrect-project
             rubocop-autocorrect-current-file))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'")

(use-package slim-mode
  :ensure t
  :mode "\\.slim\\'")

(use-package rbenv
  :ensure t
  :diminish rbenv-mode
  :commands (global-rbenv-mode rbenv-use-corresponding)
  :init
  (add-hook 'enh-ruby-mode-hook 'global-rbenv-mode)
  (add-hook 'enh-ruby-mode-hook 'rbenv-use-corresponding)
  (setq rbenv-modeline-function 'rbenv--modeline-plain))

(use-package rspec-mode
  :ensure t
  :defer t
  :init
  (setq rspec-autosave-buffer t)
  (add-hook 'haml-mode-hook 'rspec-mode)
  (add-hook 'slim-mode-hook 'rspec-mode))

(use-package yard-mode
  :diminish yard-mode
  :ensure t
  :defer t
  :init
  (add-hook 'enh-ruby-mode-hook 'yard-mode))

(use-package minitest
  :ensure t
  :defer t
  :init
  (setq minitest-keymap-prefix (kbd "C-c m"))
  (add-hook 'enh-ruby-mode-hook 'minitest-mode)
  :config
  (minitest-install-snippets))

(provide 'ag-ruby)
