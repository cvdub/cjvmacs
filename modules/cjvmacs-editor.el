;;; cjvmacs-editor.el --- Editor config for CJVmacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Christian Vanderwall

;; Author: Christian Vanderwall <christian@cvdub.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Editor config for CJVmacs

;;; Code:

(use-package cus-edit
  :config
  (add-hook 'after-init-hook (lambda () (load custom-file t)))
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (user-mail-address "christian@cvdub.net"))

(use-package emacs
  :config
  ;; Use zap-up-to-char instead of zap-to-char.
  (substitute-key-definition #'zap-to-char #'zap-up-to-char global-map)

  ;; Use DWIM functions to change case
  (substitute-key-definition #'upcase-word #'upcase-dwim global-map)
  (substitute-key-definition #'downcase-word #'downcase-dwim global-map)
  (substitute-key-definition #'capitalize-word #'capitalize-dwim global-map)

  ;; Replace fill-paragraph with fill/unfill version
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

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (defalias 'list-buffers 'ibuffer)

  :custom
  ;; Editor config
  (sentence-end-double-space nil)
  (load-prefer-newer t)
  (initial-scratch-message nil)
  (tab-always-indent 'complete)
  (disabled-command-function nil)

  ;; Autosave
  (auto-save-no-message t)
  (remote-file-name-inhibit-auto-save t)
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (auto-save-list-file-prefix (expand-file-name "auto-save-list/saves-" user-emacs-cache-directory))

  ;; Backups
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (create-lockfiles nil))

(use-package simple
  :custom
  (indent-tabs-mode nil)
  (save-interprogram-paste-before-kill t)
  (kill-whole-line t))

(use-package auth-source
  :defer t
  :custom (auth-sources (list (expand-file-name "authinfo.gpg" user-emacs-local-directory))))

(use-package bookmark
  :custom
  (bookmark-file (expand-file-name "bookmarks" user-emacs-local-directory)))

(use-package project
  :defer t
  :bind (:map project-prefix-map
              ("r" . #'project-recompile)
              ("R" . #'project-query-replace-regexp))
  :custom
  (project-list-file (expand-file-name "projects" user-emacs-local-directory)))

(use-package transient
  :defer t
  :custom
  (transient-history-file (expand-file-name "transient/history.el" user-emacs-local-directory))
  (transient-levels-file (expand-file-name "transient/levels.el" user-emacs-local-directory))
  (transient-values-file (expand-file-name "transient/values.el" user-emacs-local-directory)))

(use-package delsel
  :custom
  (delete-selection-mode t))

(use-package files
  :custom
  (require-final-newline t)
  (view-read-only t))

(use-package autorevert
  :custom
  (global-auto-revert-mode t))

(use-package ffap
  :defer t
  :hook (after-init-hook . ffap-bindings))

(use-package editorconfig
  :custom
  (editorconfig-mode t))

(use-package elec-pair
  :custom
  (electric-pair-mode t))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-isearch-open nil))

(use-package treesit
  :init
  (setq treesit-language-source-alist '((python "https://github.com/tree-sitter/tree-sitter-python")
                                        (rust "https://github.com/tree-sitter/tree-sitter-rust")
                                        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                                        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
                                        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
                                        (json "https://github.com/tree-sitter/tree-sitter-json")))
  :custom
  (treesit-extra-load-path (list (expand-file-name "tree-sitter"
                                                   user-emacs-local-directory))))

(use-package apheleia
  :ensure t
  :diminish apheleia-mode
  :init (apheleia-global-mode +1)
  :bind (:map cjv/toggle-map
              ("f" . #'apheleia-mode)
              :map cjv/code-map
              ("f" . #'apheleia-format-buffer))
  :config
  (dolist (formatter
           '((djlint . ("djlint"
                        filepath
                        "--reformat"
                        "--format-css"
                        "--format-js"
                        "--quiet"
                        "--profile"
                        "django"))
             (djade . ("uvx" "djade" filepath))
             (ruff-sort-imports . ("ruff" "check" "-" "--select" "I" "--fix" "--quiet"))
             (ruff . ("ruff" "format" "-" "--quiet"))))
    (push formatter apheleia-formatters))
  (dolist (major-mode '(python-mode python-ts-mode))
    (setf (alist-get major-mode apheleia-mode-alist)
          (list 'ruff-sort-imports 'ruff))))

(use-package eglot
  :ensure t
  :defer t
  :hook (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
  :custom
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-ignored-server-capabilities :documentOnTypeFormatting))

(use-package repeat
  :config
  (put 'other-window 'repeat-map nil)   ; Disable repeat for other-window
  :custom
  (repeat-mode t))

(use-package window
  :bind (:map window-prefix-map
              ("w" . #'window-swap-states))
  :custom
  (split-height-threshold 90))

(use-package subword
  :diminish subword-mode
  :custom
  (global-subword-mode t))

(use-package multiple-cursors
  :ensure t
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
         ("C-e" . #'mc/edit-ends-of-lines))
  :custom
  (mc/list-file (expand-file-name "mc-lists.el" user-emacs-local-directory)))

(use-package avy
  :ensure t
  :bind ("C-'" . avy-goto-subword-1)
  :custom
  (avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))
  (avy-all-windows nil))

(use-package dired
  :hook ((dired-mode . turn-on-gnus-dired-mode)
         (dired-mode . (lambda () (setq truncate-lines t))))
  :defer t
  :custom
  (dired-auto-revert-buffer t)
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t)
  (dired-listing-switches "-alh")
  (dired-mouse-drag-files t))

(use-package dired-aux
  :after dired
  :config
  (add-to-list 'dired-compress-files-alist '("\\.tar\\'" . "tar -cf %o %i")))

(use-package dired-x
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

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(use-package dired-rsync
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(use-package rg
  :ensure t
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

(use-package goto-last-change
  :ensure t
  :defer t
  :bind ("M-g l" . #'goto-last-change)
  (:repeat-map goto-last-change-repeat-map
               ("l" . goto-last-change)))

(use-package expand-region
  :ensure t
  :defer t
  :bind ("M-2" . 'er/expand-region))

(use-package envrc
  :ensure t
  :diminish envrc-mode
  :init
  (envrc-global-mode))

(use-package flyspell
  :diminish flyspell-mode
  :defer t
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

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

(use-package profiler
  :defer t
  :commands (cjv/profiler-toggle)
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

(use-package flymake
  :defer t
  :hook prog-mode
  :bind (:map flymake-mode-map
              ("M-n" . #'flymake-goto-next-error)
              ("M-p" . #'flymake-goto-prev-error))
  :custom
  (flymake-mode-line-format '(" "
                              flymake-mode-line-exception
                              flymake-mode-line-counters)))

(use-package ispell
  :defer t
  :custom
  (ispell-dictionary "en")
  (ispell-personal-dictionary "~/.config/aspell/personal-dictionary.pws"))

(use-package url-cache
  :defer t
  :custom
  (url-cache-directory (expand-file-name "url/cache" user-emacs-cache-directory)))

(use-package markdown-mode
  :ensure t)

(use-package help
  :bind (:map help-map
              ("'" . #'describe-char)))

(use-package imenu
  :defer t
  :custom
  (imenu-flatten t))

(use-package yasnippet
  :ensure t
  :defer t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  (yas-reload-all))

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

(provide 'cjvmacs-editor)

;;; cjvmacs-editor.el ends here
