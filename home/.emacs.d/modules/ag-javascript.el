(require 'use-package)

(use-package js2-mode
  :ensure t
  :init
  (setq js-indent-level 2)
  (setq js2-basic-offset 2)
  (setq js2-highlight-level 3)
  (setq js2-mode-assume-strict t)
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
  :mode "\\.js\\'")

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'")

(provide 'ag-javascript)
