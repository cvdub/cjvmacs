(use-package transient)

(use-package magit
  :defer t
  :bind (:map magit-section-mode-map
              ("C-<tab>" . nil))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(provide 'init-git)
