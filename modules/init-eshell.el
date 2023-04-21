(use-package eshell
  :defer t
  :bind (:map cjv/open-map
              ("e" . #'eshell))
  :custom
  (eshell-banner-message ""))

(provide 'init-eshell)
