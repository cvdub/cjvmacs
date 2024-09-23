(use-package paredit
  :defer t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))

(use-package inf-lisp
  :ensure nil
  :defer t
  :custom (inferior-lisp-program "sbcl"))

(use-package sly
  :defer t
  :bind (:map sly-mode-map
              ("C-c o r" . cjv/lisp-open-repl))
  :config
  (defun cjv/lisp-open-repl ()
    "Opens sly REPL in a bottom side window."
    (interactive)
    (cjv/with-bottom-window
     (if (sly-connected-p)
         (sly-mrepl)
       (sly)))))

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(provide 'init-lisp)
