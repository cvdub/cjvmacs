(use-package eshell
  :ensure nil
  :defer t
  :commands cjv/open-eshell
  :bind (:map cjv/open-map
              ("e" . #'cjv/open-eshell))
  :hook (eshell-mode-hook . (lambda ()
                              (remove-hook 'eshell-output-filter-functions 'eshell-postoutput-scroll-to-bottom)))
  :config
  (defun cjv/open-eshell ()
    "Opens eshell in a bottom side window."
    (interactive)
    (cjv/with-bottom-window (eshell)))

  (add-to-list 'eshell-modules-list 'eshell-tramp)
  :custom
  (eshell-banner-message "")
  (eshell-banner-message "")
  (eshell-history-size 100000)
  (eshell-hist-ignoredumps t)
  (eshell-destroy-buffer-when-process-dies t))

(provide 'init-eshell)
