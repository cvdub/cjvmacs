(use-package tramp
  :elpaca nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  :custom
  (tramp-default-method "ssh")
  (tramp-backup-directory-alist nil)
  (tramp-use-ssh-controlmaster-options nil))

(provide 'init-tramp)
