(use-package paredit
  :defer t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))

(use-package inf-lisp
  :elpaca nil
  :custom (inferior-lisp-program "sbcl"))


(use-package sly
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

(provide 'init-lisp)
