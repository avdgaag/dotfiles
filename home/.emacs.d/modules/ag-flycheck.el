(require 'use-package)

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (global-flycheck-mode))

(provide 'ag-flycheck)
