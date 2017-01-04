(require 'use-package)

(use-package elm-mode
  :ensure t
  :defer t
  :init
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (setq elm-format-on-save t)
  (setq elm-indent-offset 4)
  (setq elm-tags-on-save t)
  :config
  (add-to-list 'company-backends 'company-elm))

(use-package flycheck-elm
  :ensure t
  :defer t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

(provide 'ag-elm)
