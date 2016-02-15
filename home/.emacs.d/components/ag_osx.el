;;; osx.el --- customizations for Mac OSX

;;; Commentary:

;;; Code:

;; On Mac OSX, delete files by moving them to ~/.Tash
(cond ((eq system-type 'darwin)
       (setq delete-by-moving-to-trash t)
       (setq trash-directory "~/.Trash/")))

(setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
;; Ensure we can load the actual binaries we want to use
(push "/usr/local/bin" exec-path)

(global-set-key (kbd "M-`") 'ns-next-frame)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "M-˙") 'ns-do-hide-others)

;;; osx.el ends here
