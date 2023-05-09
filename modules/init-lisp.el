(use-package paredit
  :defer t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)))

(use-package sly)

(provide 'init-lisp)
