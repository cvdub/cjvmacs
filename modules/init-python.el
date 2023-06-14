(use-package python
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :hook (python-ts-mode . eglot-ensure)
  :bind (:map python-ts-mode-map
              ("C-c o r" . cjv/python-open-shell))
  :config
  (defun cjv/python-open-shell ()
    "Opens a Python shell in a bottom side window."
    (interactive)
    (cjv/with-bottom-window (run-python nil 'project t)))
  :custom
  (python-shell-dedicated 'project)
  (python-interpreter "python3"))

(use-package pyvenv
  :init
  (pyvenv-tracking-mode 1))

(use-package poetry
  :defer t)

(use-package python-pytest
  :defer t
  :bind (:map cjv/code-map
              ("t" . #'python-pytest-dispatch)))

(provide 'init-python)
