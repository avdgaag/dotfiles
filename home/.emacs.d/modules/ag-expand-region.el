(require 'use-package)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(provide 'ag-expand-region)
