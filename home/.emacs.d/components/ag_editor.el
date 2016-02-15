;;; ag_editor.el --- TODO
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
;;; editor.el -- configure general editor settings

;;; Commentary:

;;; Code:

;; No need for backup files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

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

;; Auto-pair parentheses
(electric-pair-mode +1)

;; Enable subword-mode in all programming modes to allow navigation
;; over camelCased words
(add-hook 'prog-mode-hook 'subword-mode)

;;; editor.el ends here
