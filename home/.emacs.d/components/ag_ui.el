;;; ag_ui.el --- TODO
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
;;; ui.el -- UI customizations

;;; Commentary:

;;; Code:

;; Use Y/N intead of Yes/No
(fset 'yes-or-no-p 'y-or-n-p)

;; Keep a couple of lines of screen context when scrolling
(setq scroll-margin 3 scroll-preserve-screen-position 1)

;; Customize line spacing
(setq-default line-spacing 1)

;; Use 4 columns for line numbers and then a space
(setq linum-format "%4d ")

;; When using GUI, do not open new frames but re-use existing frames
;; when opening new files.
(setq ns-pop-up-frames nil)

;; Always show line and column numbers
(global-linum-mode 1)
(column-number-mode 1)

;; Don't show startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Hide unnecessary GUI chrome
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(show-paren-mode t)

(setq ns-function-modifier 'hyper)
(menu-bar-mode +1)

;; do not blink
(blink-cursor-mode -1)

;;; ui.el ends here
