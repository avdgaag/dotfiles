;;; ag-mu4e.el --- TODO
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
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)

(setq mu4e-maildir "~/.mail/gmail")
(setq mu4e-sent-folder "/sent")
(setq mu4e-drafts-folder "/drafts")
(setq mu4e-trash-folder "/bin")
(setq mu4e-refile-folder "/archive")
(setq mu4e-sent-messages-behaviour 'delete)
(setq mu4e-get-mail-command "offlineimap -q")
(setq mu4e-update-interval 300)
(setq mu4e-use-fancy-chars t)
(setq mu4e-html2text-command "w3m -T text/html")
(setq mu4e-headers-auto-update t)

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Gmail"
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches
                            msg :to "arjan.vandergaag@gmail.com")))
           :vars '((user-mail-address . "arjan.vandergaag@gmail.com")
                   (mu4e-maildir . "~/.mail/gmail")))
         ,(make-mu4e-context
           :name "Brightin"
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches
                            msg :to "arjan@brightin.nl")))
           :vars '((user-mail-address . "arjan@brightin.nl")
                   (mu4e-maildir . "~/.mail/brightin")))))

(setq mu4e-context-policy 'pick-first)

(provide 'ag-mu4e)
;;; ag-mu4e.el ends here
