(use-package notmuch
  :defer t
  :init
  (defvar cjv/email-sync-command "mbsync -Va && notmuch new")

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
              ("M" . #'cjv/update-email))
  :config
  (remove-hook 'notmuch-show-hook #'notmuch-show-turn-on-visual-line-mode)
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

(provide 'init-email)

