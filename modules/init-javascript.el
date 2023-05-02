(use-package typescript-ts-mode
  :straight (:type built-in)
  :defer t
  :mode ("\\.ts\\'")
  :hook (typescript-ts-mode . eglot-ensure)
  :custom
  (typescript-ts-mode-indent-offset 4))

(provide 'init-javascript)
