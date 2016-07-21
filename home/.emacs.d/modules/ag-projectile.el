(require 'use-package)

(use-package helm-projectile
  :ensure t
  :defer t)

(use-package projectile
             :diminish projectile-mode
             :ensure t
             :init
             (setq projectile-completion-system 'helm)
             (setq projectile-create-missing-test-files t)
             (projectile-global-mode)
             (helm-projectile-on))

(provide 'ag-projectile)
