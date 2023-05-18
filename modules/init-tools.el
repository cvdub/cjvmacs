(use-package ansible
  :defer t
  :hook (yaml-mode . ansible)
  :custom
  (ansible-vault-password-file nil))

(use-package pdf-tools
  :init (pdf-tools-install))

(provide 'init-tools)
