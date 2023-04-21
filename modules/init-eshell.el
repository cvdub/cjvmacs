(use-package eshell
  :defer t
  :custom
  (eshell-banner-message ""))

(global-set-key (kbd "C-c o e") #'eshell)

(provide 'init-eshell)
