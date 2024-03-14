(use-package ansible
  :defer t
  :hook (yaml-mode . ansible)
  :custom
  (ansible-vault-password-file nil))

(use-package pdf-tools
  :defer t
  :init (pdf-loader-install))

(use-package dired-rsync
  :defer t
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(use-package esup
  :defer t)

(provide 'init-tools)
