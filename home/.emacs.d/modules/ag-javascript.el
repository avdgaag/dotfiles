(require 'use-package)

(use-package js2-mode
  :ensure t
  :init
  (setq js-indent-level 2)
  (setq js2-basic-offset 2)
  (setq js2-highlight-level 3)
  (setq js2-mode-assume-strict t)
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
  :config
  (ag-setup-prettier-js)
  :mode "\\.js\\'")

(defun ag-setup-prettier-js ()
  "Set up custom prettier-js module for js2-mode."
  (require 'prettier-js)
  (setq prettier-target-mode "js2-mode")
  (add-hook 'js2-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'prettier-before-save))))

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package mocha
  :ensure t
  :init
  (setq mocha-command "node_modules/.bin/mocha")
  (setq mocha-options "--recursive --reporter dot")
  (setq mocha-project-test-directory "test")
  (setq mocha-reporter "spec")
  (setq mocha-which-node "/usr/local/bin/node")
  (setq mocha-environment-variables "NODE_ENV=test"))

(provide 'ag-javascript)
