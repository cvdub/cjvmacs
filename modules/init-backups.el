(setq backup-directory-alist `((".*" . ,(expand-file-name "backups/" user-emacs-directory)))
      backup-by-copying t
      version-control t ; create versioned backups
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

(provide 'init-backups)
