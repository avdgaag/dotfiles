;;; ag-editor.el --- customize the core editor experience
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

;; Store backup files in tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Default tab width and usage
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Set a default line width
(setq-default fill-column 80)

;; Newline at end of file
(setq require-final-newline t)

;; Delete selections with a keypress
(delete-selection-mode t)

;; Revert buffers automatically
(global-auto-revert-mode t)

;; Enable clipboard copy/paste
(setq x-select-enable-clipboard t)

;; Always use UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Automatically make scripts exectable if they contain a shebang line
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Enable subword-mode in all programming modes to allow navigation
;; over camelCased words
(add-hook 'prog-mode-hook 'subword-mode)

;; Always autoscroll compilation output, so long reuslt listings are easier to
;; read.
(setq compilation-scroll-output 'first-error)

;; automatically tail opened log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

(provide 'ag-editor)
;;; ag-editor.el ends here
