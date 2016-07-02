;;; ag-projectile.el --- TODO
;;
;; Author: Arjan van der Gaag <arjan@arjanvandergaag.nl>
;; URL: http://arjanvandergaag.nl
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; TODO

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'helm)
  (setq projectile-create-missing-test-files t)
  :config
  (projectile-global-mode)

  (defun ag-expand-completion-table (orig-fun &rest args)
    "Extract all symbols from COMPLETION-TABLE before calling projectile--tags."
    (let ((completion-table (all-completions "" (car args))))
      (funcall orig-fun completion-table)))

  (advice-add 'projectile--tags :around #'ag-expand-completion-table))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package ggtags
  :diminish ggtags-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'ggtags-mode))

(provide 'ag-projectile)
;;; ag-projectile.el ends here
