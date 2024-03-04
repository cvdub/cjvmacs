(use-package python
  :ensure nil
  :defer t
  :init
  :hook ((python-ts-mode . eglot-ensure))
  :bind (:map python-ts-mode-map
              ("C-c o r" . cjv/python-open-shell))
  :config
  (defun cjv/python-open-shell ()
    "Opens a Python shell in a bottom side window."
    (interactive)
    (cjv/with-bottom-window (run-python nil 'project t)))

  :custom
  (python-shell-dedicated 'project)
  (python-interpreter "python3")
  (python-indent-guess-indent-offset-verbose nil)
  (python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-")))

(use-package pyvenv
  :defer 5
  :init
  (pyvenv-tracking-mode 1))

(use-package poetry
  :defer t)

(use-package python-pytest
  :defer t
  :bind (:map cjv/code-map
              ("t" . #'python-pytest-dispatch)))

(use-package flymake-ruff
  :ensure (flymake-ruff :host github :repo "erickgnavar/flymake-ruff"
                        :remotes (("fork" :repo "cvdub/flymake-ruff" :protocol ssh)))
  :after eglot
  :hook (eglot-managed-mode . flymake-ruff-load)
  :config
  (defun cjv/filter-eglot-diagnostics (diags)
    "Drop Pyright variable not accessed notes."
    (list (seq-remove (lambda (d)
                        (and (eq (flymake-diagnostic-type d) 'eglot-note)
                             (s-starts-with? "Pyright:" (flymake-diagnostic-text d))
                             (s-ends-with? "is not accessed" (flymake-diagnostic-text d))))
                      (car diags))))
  (advice-add 'eglot--report-to-flymake :filter-args #'cjv/filter-eglot-diagnostics))

(provide 'init-python)
