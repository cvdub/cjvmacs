(electric-pair-mode 1)
(setq-default fill-column 80)
(setq sentence-end-double-space nil)

(use-package eldoc
  :straight (:type built-in)
  :diminish eldoc-mode)

(use-package lorem-ipsum
  :straight (lorem-ipsum :type git :host github :repo "jschaf/emacs-lorem-ipsum"
                         :fork (:host github :repo "cvdub/emacs-lorem-ipsum" :protocl ssh))
  :defer t)

(use-package multiple-cursors
  :defer t
  :init
  (defvar cjv/multiple-cursors-map (make-sparse-keymap)
    "Keymap for multiple cursors stuff.")
  (bind-key (kbd "C-m") cjv/multiple-cursors-map ctl-x-map)
  :bind (("M-3" . #'mc/mark-next-like-this)
         ("M-4" . #'mc/mark-previous-like-this)
         ("M-#" . #'mc/unmark-next-like-this)
         ("M-$" . #'mc/unmark-previous-like-this)
         :map cjv/multiple-cursors-map
         ("a" . #'mc/mark-all-dwim)
         ("d" . #'mc/mark-all-symbols-like-this-in-defun)
         ("e" . #'mc/edit-lines)
         ("i" . #'mc/insert-numbers)
         ("C-a" . #'mc/edit-beginnings-of-lines)
         ("C-e" . #'mc/edit-ends-of-lines)))

(use-package avy
  :bind ("C-'" . avy-goto-subword-1)
  :custom
  (avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o)))

(use-package dired
  :straight (:type built-in)
  :hook (dired-mode . turn-on-gnus-dired-mode)
  :custom
  (dired-auto-revert-buffer t)
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t))

(use-package dired-narrow
  :defer t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package rg
  :defer t
  :bind ("C-c s" . #'rg-menu)
  :config
  (rg-define-toggle "-g '!*migrations'" (kbd "M") t)
  (rg-define-toggle "-g '!*tests'" (kbd "T"))
  (rg-define-toggle "--context 3" (kbd "C")))

;;;; Completion
(use-package company
  :hook prog-mode
  :diminish
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  (company-transformers '(company-sort-by-occurrence
                          company-sort-by-backend-importance)))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-resize t)
  (vertico-scroll-margin 0)
  (vertico-count 12))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

;; TODO: Configure consult
;; Need to replace default functions with consult variants
(use-package consult)

(provide 'init-editor)
