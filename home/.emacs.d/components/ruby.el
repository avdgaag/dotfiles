;; For Ruby files, enable projectile and projectile-rails
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(add-hook 'ruby-mode-hook 'global-rbenv-mode)
(add-hook 'ruby-mode-hook 'rbenv-use-corresponding)
(add-hook 'ruby-mode-hook 'robe-mode)

;; Prevent ruby-mode from adding magic encoding comments to the top of files
(setq ruby-insert-encoding-magic-comment nil)
