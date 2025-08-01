;;; cjvmacs-email.el --- Email config for CJVmacs    -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Christian Vanderwall

;; Author: Christian Vanderwall <christian@cvdub.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Email config for CJVmacs

;;; Code:

(use-package notmuch
  :ensure t
  :commands notmuch-refresh-all-buffers
  :hook ((notmuch-search-mode . hl-line-mode)
         (notmuch-show-mode . variable-pitch-mode)
         (notmuch-message-mode . variable-pitch-mode))
  :init
  (setq mail-user-agent 'notmuch-user-agent)

  (defvar cjv/email-sync-command "mbsync -a && notmuch new")

  (defun cjv/notmuch-inbox ()
    "Opens the notmuch inbox."
    (interactive)
    (notmuch-search "tag:inbox" t))

  (defun cjv/update-email ()
    "Sync emails with server."
    (interactive)
    (let ((compilation-buffer-name-function (lambda (_) "*email update*")))
      (with-current-buffer (compile cjv/email-sync-command)
        (add-hook
         'compilation-finish-functions
         (lambda (buf status)
           (if (equal status "finished\n")
               (progn
                 (delete-windows-on buf)
                 (bury-buffer buf)
                 (notmuch-refresh-all-buffers)
                 (message "Email sync successful"))
             (user-error "Failed to sync emails")))
         nil
         'local))))

  (defconst cjv/message-cite-style-gmail
    '((message-cite-function  'message-cite-original)
      (message-citation-line-function  'message-insert-formatted-citation-line)
      (message-cite-reply-position 'above)
      (message-yank-prefix  "> ")
      (message-yank-cited-prefix  ">")
      (message-yank-empty-prefix  ">")

      (message-citation-line-format "On %a, %b %e, %Y at %R %p %f wrote:\n"))
    "Message citation style used by Gmail.  Use with `message-cite-style'.")

  (defun cjv/toggle-message-cite-style ()
    "Toggle message-cite-style between bottom posting and gmail."
    (interactive)
    (if message-cite-style
        (progn
          (message "Message cite style switched to: bottom posting")
          (setq message-cite-style nil))
      (progn
        (message "Message cite style switched to: Gmail")
        (setq message-cite-style 'cjv/message-cite-style-gmail))))

  :hook ((notmuch-message-mode . visual-line-fill-column-mode)
         (notmuch-message-mode . turn-off-auto-fill))
  :bind (:map cjv/open-map
              ("m" . #'cjv/notmuch-inbox)
              ("M" . #'cjv/update-email)
              :map notmuch-show-mode-map
              ;; ("<RET>" . #'cjv/notmuch-browse-url-or-notmuch-show-toggle-message)
              ;; ("q" . #'cjv/notmuch-bury-or-kill-this-buffer-maybe-delete-window)
              :map cjv/toggle-map
              ("m" . #'cjv/toggle-message-cite-style))
  :config
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)
  (add-hook 'message-send-hook #'notmuch-mua-attachment-check)

  (defun cjv/notmuch-mua-empty-subject-check ()
    "Request confirmation before sending a message with empty subject."
    (when (and (null (message-field-value "Subject"))
               (not (y-or-n-p "Subject is empty, send anyway? ")))
      (error "Sending message cancelled: empty subject")))
  (add-hook 'message-send-hook #'cjv/notmuch-mua-empty-subject-check)

  (defun cjv/notmuch-browse-url-or-notmuch-show-toggle-message ()
    "Browse URL at point or toggle message at point."
    (interactive)
    (if (thing-at-point 'url t)
        (browse-url-at-point)
      (notmuch-show-toggle-message)))

  (defun cjv/message-recipient-first-name ()
    "Returns the first name of first email recipient, or nil if not found."
    (interactive)
    (save-excursion
      (message-goto-to)
      (beginning-of-line)
      (when (re-search-forward "To: \\([^<]+\\) <.*>$" nil t)
        (car (split-string (match-string-no-properties 1))))))

  (defun cjv/message-cite-function ()
    (let ((bindings (if (symbolp message-cite-style)
	                (symbol-value message-cite-style)
	              message-cite-style)))
      (cl-progv (mapcar #'car bindings)
          (mapcar (lambda (binding) (eval (cadr binding) t)) bindings)
        (message-cite-original))))

  (setq notmuch-mua-cite-function #'cjv/message-cite-function)

  (defun cjv/notmuch-search-show-thread-maybe-new-window (&rest args)
    "Create a new window and switch to it if there is only one window open."
    (when (= (length (window-list)) 1)
      (split-window-right)
      (other-window 1)))

  ;; (advice-add 'notmuch-search-show-thread
  ;;             :before #'cjv/notmuch-search-show-thread-maybe-new-window)

  (defun cjv/notmuch-bury-or-kill-this-buffer-maybe-delete-window ()
    "Delete window after killing notmuch-show buffer if notmuch-search is open in the other window."
    (interactive)
    (notmuch-bury-or-kill-this-buffer)
    (when (and (= (length (window-list)) 2)
               (seq-some (lambda (window)
                           (unless (eq window (selected-window))
                             (with-selected-window window
                               (eq major-mode 'notmuch-search-mode))))
                         (window-list)))
      (delete-window)
      (notmuch-refresh-this-buffer)))

  (defun cjv/notmuch-show-archive-message-refresh-notmuch-search ()
    "Refresh notmuch-search buffer after archiving a message."
    (walk-windows
     (lambda (window)
       (with-current-buffer (window-buffer window)
         (when (eq major-mode 'notmuch-search-mode)
           (notmuch-refresh-this-buffer))))))

  ;; (advice-add 'notmuch-show-archive-message-then-next-or-next-thread
  ;;             :after #'cjv/notmuch-show-archive-message-refresh-notmuch-search)

  (add-to-list 'display-buffer-alist
               '((major-mode . notmuch-show-mode)
                 (inhibit-same-window . t)))

  ;; Inline images
  (defun cjv/notmuch--create-responsive-image (image-data &optional max-width)
    (let* ((max-width (or max-width (truncate (* (window-pixel-width) 0.95))))
           (image (create-image image-data nil t))
           (width (car (image-size image t))))
      (if (> width max-width)
          (create-image image-data nil t :width max-width)
        image)))

  (defun cjv/notmuch-show-inline-cid-images (&optional _msg _depth)
    "Replace [cid:...] references with inline images in notmuch-show."
    (interactive)
    (message "Replacing inline images!")
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[cid:\\(.*\\.[png|jpg].*\\)\\]" nil t)
        (let* ((cid (match-string 1))
               (image-data (notmuch-show--get-cid-content cid))) ;; Retrieve the attachment
          (when image-data
            (replace-match "")
            (insert-image (cjv/notmuch--create-responsive-image (car image-data))))))))

  (add-hook 'notmuch-show-hook #'cjv/notmuch-show-inline-cid-images)

  :custom
  (message-directory "~/.mail/")
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (sendmail-program "/opt/homebrew/bin/msmtp")
  (mail-specify-envelope-from t)
  (message-sendmail-envelope-from 'header)
  (mail-envelope-from 'header)
  (notmuch-fcc-dirs '(("christian@cvdub.net" . "christian@cvdub.net/Sent -inbox -unread -new +sent")
                      ("christian@spacebaseapp.com" . "christian@spacebaseapp.com/Sent -inbox -unread -new +sent")
                      ("cvanderwall14@gmail.com" . "cvanderwall14@gmail.com/Sent -inbox -unread -new +sent")
                      ("christian@vanderwall.org" . "christian@vanderwall.org/Sent -inbox -unread -new +sent")))
  (message-citation-line-function 'message-insert-formatted-citation-line)
  (notmuch-mua-compose-in 'new-window)
  (notmuch-wash-wrap-lines-length 80)
  (notmuch-search-oldest-first nil)
  (notmuch-archive-tags '("-inbox" "-unread"))
  (notmuch-multipart/alternative-discouraged '("text/x-amp-html"
                                               "text/calendar"
                                               "text/html"
                                               "multipart/related"))
  (notmuch-show-text/html-blocked-images nil)
  ;; (mm-text-html-renderer 'shr)
  ;; (notmuch-show-text/html-blocked-images nil)
  ;; (notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))
  ;; (shr-use-colors nil)
  ;; (shr-use-fonts nil)
  ;; (shr-max-width fill-column)
  )

(use-package org-mime
  :ensure t
  :after notmuch
  :bind (:map notmuch-message-mode-map
              ("C-c M-o" . #'org-mime-htmlize)))

(use-package gnus-alias
  :ensure t
  :init (add-hook 'message-setup-hook #'gnus-alias-determine-identity)
  :bind (:map notmuch-message-mode-map
              ("C-c C-f o" . gnus-alias-select-identity))
  :config
  (setq gnus-alias-default-identity "christian@cvdub.net"
        gnus-alias-identity-alist
        '(("christian@cvdub.net"
           nil ;; Does not refer to any other identity
           "Christian Vanderwall <christian@cvdub.net>"
           nil ;; No organization header
           (("Fcc" . "christian@cvdub.net/Sent -inbox -unread -new +sent"))
           nil ;; No extra body text
           "~/.mail/christian@cvdub.net/.signature")
          ("christian@spacebaseapp.com"
           nil ;; Does not refer to any other identity
           "Christian Vanderwall <christian@spacebaseapp.com>"
           "Spacebase"
           (("Fcc" . "christian@spacebaseapp.com/Sent -inbox -unread -new +sent"))
           nil ;; No extra body text
           "~/.mail/christian@spacebaseapp.com/.signature")
          ("cvanderwall14@gmail.com"
           nil ;; Does not refer to any other identity
           "Christian Vanderwall <cvanderwall14@gmail.com>"
           nil ;; No organization header
           (("Fcc" . "cvanderwall14@gmail.com/Sent -inbox -unread -new +sent"))
           nil ;; No extra body text
           "~/.mail/cvanderwall14@gmail.com/.signature")
          ("christian@vanderwall.org"
           nil ;; Does not refer to any other identity
           "Christian Vanderwall <christian@vanderwall.org>"
           nil ;; No organization header
           (("Fcc" . "christian@vanderwall.org/Sent -inbox -unread -new +sent"))
           nil ;; No extra body text
           "~/.mail/christian@vanderwall.org/.signature"))
        gnus-alias-identity-rules
        '(("christian@cvdub.net" ("any" "christian@cvdub.net" both) "christian@cvdub.net")
          ("christian@spacebaseapp.com" ("any" "christian@spacebaseapp.com" both) "christian@spacebaseapp.com")
          ("cvanderwall14@gmail.com" ("any" "cvanderwall14@gmail.com" both) "cvanderwall14@gmail.com")
          ("christian@vanderwall.org" ("any" "christian@vanderwall.org" both) "christian@vanderwall.org"))))

(use-package shr
  :ensure nil
  :defer t
  :custom
  (shr-use-colors nil)
  (shr-use-fonts t)
  (shr-max-width 70)
  (shr-width 70)
  (shr-discard-aria-hidden t)
  (shr-use-xwidgets-for-media t)
  (shr-cookie-policy t)
  :custom-face
  (shr-text ((t (:inherit variable-pitch)))))

(use-package ol-notmuch
  :ensure t
  :defer t
  :after org)

(provide 'cjvmacs-email)

;;; cjvmacs-email.el ends here
