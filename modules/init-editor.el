(electric-pair-mode 1)
(setq-default fill-column 80)
(setq sentence-end-double-space nil)

(use-package eldoc
  :straight (:type built-in)
  :diminish eldoc-mode)

(use-package lorem-ipsum
  :straight (lorem-ipsum :type git :host github :repo "jschaf/emacs-lorem-ipsum"
                         :fork (:host github :repo "cvdub/emacs-lorem-ipsum" :protocl ssh))
  :defer t)

(use-package company
  :hook prog-mode
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-transformers '(company-sort-by-occurrence
                          company-sort-by-backend-importance)))

(provide 'init-editor)
