(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-resize t)
  (vertico-scroll-margin 0)
  (vertico-count 12))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

;; TODO: Configure consult
;; Need to replace default functions with consult variants
(use-package consult)

(provide 'init-completion)
