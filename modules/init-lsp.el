(use-package eglot
  :straight (:type built-in)
  :bind (:map cjv/code-map
              ("f" . #'eglot-format-buffer))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t))

(provide 'init-lsp)
