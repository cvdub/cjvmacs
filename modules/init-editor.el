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

(provide 'init-editor)

