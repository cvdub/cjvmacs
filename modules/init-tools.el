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

(use-package verb
  :after org
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(provide 'init-tools)
