(use-package js2-mode
  :commands js2-mode
  :ensure t
  :init
  (setq js-curly-indent-offset 2)
  (setq js-enabled-frameworks (quote (javascript)))
  (setq js-expr-indent-offset 2)
  (setq js-indent-level 2)
  (setq js-paren-indent-offset 2)
  (setq js-square-indent-offset 2)
  (setq js2-basic-offset 2)
  :mode ("\\.js\\'"
         "\\.jsx\\'"))

(use-package coffee-mode
  :commands coffee-mode
  :ensure t
  :mode "\\.cjsx\\'")

;;; javascript.el ends here
