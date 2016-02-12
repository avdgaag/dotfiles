;;; javascript.el --- configure javacript

;;; Commentary:

;;; Code:

(setq js-curly-indent-offset 2)
(setq js-enabled-frameworks (quote (javascript)))
(setq js-expr-indent-offset 2)
(setq js-indent-level 2)
(setq js-paren-indent-offset 2)
(setq js-square-indent-offset 2)
(setq js2-basic-offset 2)

;; Always use js2-mode for editing javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.cjsx\\'" . coffee-mode))


;;; javascript.el ends here
