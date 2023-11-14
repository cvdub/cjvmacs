(use-package notmuch
  :defer t
  :init
  (defvar cjv/email-sync-command "parallel mbsync -V ::: christian@cvdub.net christian@spacebaseapp.com christian@vanderwall.org cvanderwall14@gmail.com && notmuch new")

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

  :hook ((notmuch-show-mode . mixed-pitch-mode)
         (notmuch-message-mode . mixed-pitch-mode)
         (notmuch-message-mode . visual-line-mode)
         (notmuch-message-mode . turn-off-auto-fill))
  :bind (:map cjv/open-map
              ("m" . #'cjv/notmuch-inbox)
              ("M" . #'cjv/update-email)
              :map notmuch-show-mode-map
              ("<RET>" . #'cjv/notmuch-browse-url-or-notmuch-show-toggle-message))
  :config
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)

  (defun cjv/notmuch-browse-url-or-notmuch-show-toggle-message ()
    "Browse URL at point or toggle message at point."
    (interactive)
    (if (thing-at-point 'url t)
        (browse-url-at-point)
      (notmuch-show-toggle-message)))

  :custom
  (message-directory "~/.mail/")
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
  ;; (mm-text-html-renderer 'shr)
  ;; (notmuch-show-text/html-blocked-images nil)
  ;; (notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))
  ;; (shr-use-colors nil)
  ;; (shr-use-fonts nil)
  ;; (shr-max-width fill-column)
  )

(use-package org-mime
  :after notmuch
  :bind (:map notmuch-message-mode-map
              ("C-c M-o" . #'org-mime-htmlize)))

(use-package gnus-alias
  :after notmuch
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
           "~/.signature")
          ("christian@spacebaseapp.com"
           nil ;; Does not refer to any other identity
           "Christian Vanderwall <christian@spacebaseapp.com>"
           "Spacebase"
           (("Fcc" . "christian@spacebaseapp.com/Sent -inbox -unread -new +sent"))
           nil ;; No extra body text
           "~/.signature")
          ("cvanderwall14@gmail.com"
           nil ;; Does not refer to any other identity
           "Christian Vanderwall <cvanderwall14@gmail.com>"
           nil ;; No organization header
           (("Fcc" . "cvanderwall14@gmail.com/Sent -inbox -unread -new +sent"))
           nil ;; No extra body text
           "~/.signature")
          ("christian@vanderwall.org"
           nil ;; Does not refer to any other identity
           "Christian Vanderwall <christian@vanderwall.org>"
           nil ;; No organization header
           (("Fcc" . "christian@vanderwall.org/Sent -inbox -unread -new +sent"))
           nil ;; No extra body text
           "~/.signature"))
        gnus-alias-identity-rules
        '(("christian@cvdub.net" ("any" "christian@cvdub.net" both) "christian@cvdub.net")
          ("christian@spacebaseapp.com" ("any" "christian@spacebaseapp.com" both) "christian@spacebaseapp.com")
          ("cvanderwall14@gmail.com" ("any" "cvanderwall14@gmail.com" both) "cvanderwall14@gmail.com")
          ("christian@vanderwall.org" ("any" "christian@vanderwall.org" both) "christian@vanderwall.org"))))

(provide 'init-email)

