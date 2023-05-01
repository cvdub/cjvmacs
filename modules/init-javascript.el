(use-package typescript-ts-mode
  :straight (:type built-in)
  :defer t
  :mode ("\\.ts\\'")
  :hook (typescript-ts-mode . eglot-ensure))

(provide 'init-javascript)
