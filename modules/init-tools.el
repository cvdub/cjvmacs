(use-package ansible
  :defer t
  :hook (yaml-mode . ansible)
  :custom
  (ansible-vault-password-file nil))

(use-package pdf-tools
  :defer t
  :config (pdf-tools-install :no-query))

(use-package dired-rsync
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(use-package esup
  :defer t)

(provide 'init-tools)
