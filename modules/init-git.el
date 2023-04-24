(use-package magit
  :defer t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(provide 'init-git)
