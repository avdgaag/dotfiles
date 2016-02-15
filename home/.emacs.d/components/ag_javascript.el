;;; ag_javascript.el --- TODO
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
