;;; asciidoc.el --- customizations for AsciiDoc

;;; Commentary:

;;; Code:

;; Treat .adoc files as AsciiDoc automatically
(add-to-list 'auto-mode-alist (cons "\\.adoc \\'" 'adoc-mode))

;;; asciidoc.el ends here
