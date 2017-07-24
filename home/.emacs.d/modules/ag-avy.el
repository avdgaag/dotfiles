(require 'use-package)

(use-package avy
  :ensure t
  :bind ("s-." . avy-goto-word-or-subword-1))

(provide 'ag-avy)
