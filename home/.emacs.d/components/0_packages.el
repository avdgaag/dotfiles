(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
       '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq package-list
      '(use-package))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
