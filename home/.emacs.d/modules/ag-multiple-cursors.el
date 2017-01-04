(require 'use-package)

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m" . mc/edit-lines)
         ("C-c M" . mc/edit-ends-of-lines)
         ("s-]" . mc/mark-next-like-this)
         ("s-[" . mc/mark-previous-like-this)
         ("s-}" . mc/mark-all-like-this)
         ("s-{" . mc/mark-all-dwim)
         )
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

(provide 'ag-multiple-cursors)
