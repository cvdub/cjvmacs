(use-package eshell
  :defer t
  :commands cjv/open-eshell
  :bind (:map cjv/open-map
              ("e" . #'cjv/open-eshell))
  :config
  (defun cjv/open-eshell ()
    "Opens eshell in a bottom side window."
    (interactive)
    (cjv/with-bottom-window (eshell)))
  :custom
  (eshell-banner-message ""))

(provide 'init-eshell)
