(use-package ansible
  :defer t
  :hook (yaml-mode . ansible)
  :custom
  (ansible-vault-password-file nil))

(use-package pdf-tools
  :init (pdf-tools-install))

(use-package dired-rsync
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(provide 'init-tools)
