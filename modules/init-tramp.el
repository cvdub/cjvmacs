(use-package tramp
  :straight (:type built-in)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  :custom
  (tramp-default-method "ssh")
  (tramp-backup-directory-alist nil))

(provide 'init-tramp)
