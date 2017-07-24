(require 'use-package)

(use-package company
  :ensure t
  :diminish company-mode
  :defer t
  :init
  (setq company-backends
        (quote
         (company-elisp
          company-css
          company-semantic
          company-etags
          company-files)))
  (setq completion-at-point-functions '(company-complete-common))
  (add-hook 'after-init-hook 'global-company-mode))

(provide 'ag-company)
