(electric-pair-mode 1)
(setq-default fill-column 80)
(setq sentence-end-double-space nil)
(repeat-mode 1)
(setq mac-system-move-file-to-trash-use-finder t)

;; Move buffer line to the top, middle, then bottom when re-centering the current buffer line.
(setq recenter-positions '(top middle bottom))

;; Use ibuffer instead of list-buffers.
(defalias 'list-buffers 'ibuffer)

;; Add keybinding for swapping windows
(bind-key (kbd "w") #'window-swap-states 'window-prefix-map)

;; Activate view-mode in read-only buffers
(setq view-read-only t)

;; Save minibuffer history
(savehist-mode)

;; Load newest version of files
(setq load-prefer-newer t)

;; Save existing clipboard text (from other programs) onto the kill ring before
;; replacing it. Ensures that Emacs kill operations do not irrevocably overwrite
;; existing clipboard text.
(setq save-interprogram-paste-before-kill t)

;; Return to last visited location when reopening buffers
(save-place-mode 1)

;; Enable all commands.
(setq disabled-command-function nil)

;; Enable subword mode
(global-subword-mode 1)

;; Use zap-up-to-char instead of zap-to-char.
(substitute-key-definition #'zap-to-char #'zap-up-to-char global-map)

;; Kill entire line when cursor is all the way left
(setq kill-whole-line t)

;; Changing case
(substitute-key-definition #'upcase-word #'upcase-dwim global-map)
(substitute-key-definition #'downcase-word #'downcase-dwim global-map)
(substitute-key-definition #'capitalize-word #'capitalize-dwim global-map)

;; Disable native comp warnings
(setq native-comp-async-report-warnings-errors nil)

;; Enable recent files mode
(add-hook 'elpaca-after-init-hook #'recentf-mode)

;; Disable scratch message
(setq initial-scratch-message nil)

(defun cjv/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'cjv/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(substitute-key-definition #'fill-paragraph #'cjv/fill-or-unfill global-map)

;;;; Lorem ipsum
(use-package lorem-ipsum
  :ensure (lorem-ipsum :host github :repo "jschaf/emacs-lorem-ipsum"
                       :remotes (("fork" :repo "cvdub/emacs-lorem-ipsum" :protocl ssh)))
  :defer t)

;;;; Multiple cursors
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

;;;; Dired
(use-package dired
  :ensure nil
  :hook ((dired-mode . turn-on-gnus-dired-mode)
         (dired-mode . (lambda () (toggle-truncate-lines 1))))
  :custom
  (dired-auto-revert-buffer t)
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t)
  (dired-listing-switches "-alh")
  (dired-mouse-drag-files t))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-files (concat dired-omit-files
                                 "\\|^\\.DS_Store\\'"
                                 "\\|^\\.project\\(?:ile\\)?\\'"
                                 "\\|^\\.\\(?:svn\\|git\\)\\'"
                                 "\\|^\\.ccls-cache\\'"
                                 "\\|\\(?:\\.js\\)?\\.meta\\'"
                                 "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"
                                 "\\|~\\$.*\\.\\(xls\\|xlsx\\|csv\\)\\'")))

(use-package dired-du
  :after dired
  :bind (:map dired-mode-map
              ("C-c l u" . dired-du-mode))
  :custom
  (dired-du-size-format t))

(use-package dired-narrow
  :defer t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package rg
  :defer t
  :commands rg-menu
  :bind (("C-c s" . #'rg-menu)
         :map rg-mode-map
         ("<S-return>" . #'cjv/compile-goto-error-same-window))
  :config
  (defun cjv/compile-goto-error-same-window ()
    "Run compile-goto-error but open in same window."
    (interactive)
    (cjv/with-same-window
     (compile-goto-error)))

  (rg-define-toggle "-g '!*migrations'" (kbd "M") t)
  (rg-define-toggle "-g '!*tests'" (kbd "T"))
  (rg-define-toggle "--context 3" (kbd "C")))

;;;; Completion
(setq tab-always-indent 'complete)

(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.33)
  (corfu-on-exact-match 'show))

(use-package cape
  :init
  (dolist (f (list #'cape-dabbrev #'cape-file #'cape-elisp-block))
    (add-to-list 'completion-at-point-functions f)))

(use-package vertico
  :ensure (vertico :files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  :custom
  (vertico-resize nil)
  (vertico-scroll-margin 0)
  (vertico-count 12))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ;; ("C-c M-x" . consult-mode-command)
         ;; ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ;; ("C-c m" . consult-man)
         ;; ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)           ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)     ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)     ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)  ;; orig. next-matching-history-element
         ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

;;;; Hideshow mode
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-isearch-open nil))

;;;; Goto last change
(use-package goto-last-change
  :defer t
  :bind ("M-g l" . #'goto-last-change)
  (:repeat-map goto-last-change-repeat-map
               ("l" . goto-last-change)))

;;;; Snippets
(use-package yasnippet
  :defer 5
  :config
  (yas-global-mode 1)
  :custom
  (yas-verbosity 2))

(use-package yasnippet-snippets
  :after yasnipped)

;;;; Ediff
(use-package ediff
  :ensure nil
  :defer t
  :config
  (defun cjv/ediff-copy-both-to-C ()
    "Copies Ediff contents of A and B to C."
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

  (defun cjv/ediff-add-d-to-mode-map ()
    (define-key ediff-mode-map "d" 'cjv/ediff-copy-both-to-C))

  (add-hook 'ediff-keymap-setup-hook 'cjv/ediff-add-d-to-mode-map)

  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

;;;; Expand region
(use-package expand-region
  :defer t
  :bind ("M-2" . 'er/expand-region))

;;;; CSV
(use-package csv-mode
  :defer t)

;;;; YAML
(use-package yaml-mode
  :defer t)

;;;; Whitespace
(use-package ws-butler
  :defer t
  :hook prog-mode)

;;;; Sudo edit
(use-package sudo-edit
  :defer t)

;;;; Envrc
(use-package envrc
  :init (envrc-global-mode))

;;;; Flyspell
(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;;;; Apheleia (auto formatting)
(use-package apheleia
  :init (apheleia-global-mode +1)
  :config
  (push '(djlint . ("djlint"
                    filepath
                    "--reformat"
                    "--format-css"
                    "--format-js"
                    "--quiet"
                    "--profile"
                    "django"))
        apheleia-formatters))

;;;; Explain Pause Mode
;; (use-package explain-pause-mode
;;   :ensure (explain-pause-mode :host github :repo "lastquestion/explain-pause-mode")
;;   :config
;;   (explain-pause-mode))

;;;; Helpful
(use-package helpful
  :defer t
  :bind (("C-h f" . #'helpful-callable)
         ("C-h k" . #'helpful-key)
         ("C-h v" . #'helpful-variable)
         ("C-h x" . #'helpful-command)))

;;;; Toggle parenthesis
(defun cjv/toggle-parens ()
  "Toggle parens at cursor."
  (interactive)
  (let ((parens (funcall show-paren-data-function)))
    (if parens
        (let* ((start (if (< (nth 0 parens) (nth 2 parens))
                          (nth 0 parens) (nth 2 parens)))
               (end (if (< (nth 0 parens) (nth 2 parens))
                        (nth 2 parens) (nth 0 parens)))
               (startchar (buffer-substring-no-properties start (1+ start)))
               (mismatch (nth 4 parens)))
          (cl-flet ((replace-parens (pair start end)
                      (goto-char start)
                      (delete-char 1)
                      (insert (substring pair 0 1))
                      (goto-char end)
                      (delete-char 1)
                      (insert (substring pair 1 2))))
            (save-excursion
              (pcase startchar
                ("(" (replace-parens "[]" start end))
                ("[" (replace-parens "{}" start end))
                ("{" (replace-parens "()" start end))))
            (pcase (char-after)
              (?\) (forward-char))
              (?\] (forward-char))
              (?\} (forward-char)))))
      (message "No parens found"))))

(bind-key (kbd "p") #'cjv/toggle-parens 'cjv/code-map)

(defvar-keymap cjv/toggle-parens-repeat-map
  :repeat t
  "p" #'cjv/toggle-parens)

;;;; Tree-sitter
(use-package treesit-auto
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt))

;;;; Combobulate
(use-package combobulate
  :defer t
  :ensure (combobulate :host github :repo "mickeynp/combobulate")
  :preface (setq combobulate-key-prefix "C-c b")
  ;; :hook ((python-ts-mode . combobulate-mode)
  ;;        (js-ts-mode . combobulate-mode)
  ;;        (html-ts-mode . combobulate-mode)
  ;;        (css-ts-mode . combobulate-mode)
  ;;        (yaml-ts-mode . combobulate-mode)
  ;;        (typescript-ts-mode . combobulate-mode)
  ;;        (json-ts-mode . combobulate-mode)
  ;;        (tsx-ts-mode . combobulate-mode))
  )


;;;; Profiler
(use-package profiler
  :ensure nil
  :commands (cjv/toggle-map)
  :bind (:map cjv/toggle-map
              ("p" . #'cjv/profiler-toggle))
  :config
  (defun cjv/profiler-toggle ()
    "Toggles the profiler."
    (interactive)
    (if (profiler-running-p)
        (progn
          (profiler-stop)
          (profiler-report))
      (call-interactively #'profiler-start))))

(provide 'init-editor)
