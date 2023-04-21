(use-package python
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :hook (python-ts-mode . eglot-ensure))

(use-package pyvenv
  :init
  (pyvenv-tracking-mode 1))

(use-package poetry)

(provide 'init-python)
