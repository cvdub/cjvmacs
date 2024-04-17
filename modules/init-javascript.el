(use-package typescript-ts-mode
  :ensure nil
  :defer t
  :mode ("\\.ts\\'")
  :hook (typescript-ts-mode . eglot-ensure)
  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package js-mode
  :defer t
  :ensure nil
  :custom
  (js-indent-level 2))

(provide 'init-javascript)
