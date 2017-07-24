(require 'use-package)

(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package inf-clojure
  :ensure t)

(use-package cider
  :ensure t)

(provide 'ag-clojure)
