(require 'use-package)

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m" . mc/edit-lines)
         ("C-c M" . mc/edit-ends-of-lines)
         ("C-c ." . mc/mark-next-like-this)
         ("C-c ," . mc/mark-previous-like-this)
         ("C-c /" . mc/mark-all-like-this)))

(provide 'ag-multiple-cursors)
