(use-package eglot
  :elpaca nil
  :bind (:map cjv/code-map
              ("f" . #'eglot-format-buffer))
  :config
  (defvar cjv/eglot-imenu-ignored-symbols '("Variable")
    "Symbols excluded from eglot's Imenu.")

  (defun cjv/eglot-imenu-filter (symbols)
    "Filters Imenu result, excluding symbols in cjv/eglot-ignored-symbols."
    (cl-remove-if (lambda (symbol-list)
                    (member (car symbol-list) cjv/eglot-imenu-ignored-symbols))
                  symbols))

  (advice-add #'eglot-imenu :filter-return #'cjv/eglot-imenu-filter)

  ;; Configure up corfu
  (add-to-list 'completion-category-overrides '(eglot (styles orderless)))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t))

(provide 'init-lsp)
