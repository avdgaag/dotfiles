(require 'use-package)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :init
  (setq markdown-command "kramdown")
  (setq markdown-open-command "open -a 'Marked 2'"))

(provide 'ag-markdown)
