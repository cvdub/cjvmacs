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
  (load custom-file t)
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (user-mail-address "christian@cvdub.net"))

(use-package emacs
  :custom
  ;; Autosave
  (auto-save-no-message t)
  (remote-file-name-inhibit-auto-save t)
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (auto-save-list-file-prefix (expand-file-name "auto-save-list/saves-" user-emacs-cache-directory))
  ;; Backups
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (create-lockfiles nil))

(use-package bookmark
  :custom
  (bookmark-file (expand-file-name "bookmarks" user-emacs-local-directory)))

(use-package project
  :defer t
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
  (require-final-newline t))

(use-package autorevert
  :custom
  (global-auto-revert-mode t))

(use-package ffap
  :config (ffap-bindings))

(use-package editorconfig
  :custom
  (editorconfig-mode t))

(use-package elec-pair
  :custom
  (electric-pair-mode t))

(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-isearch-open nil))

(use-package treesit
  :custom
  (treesit-extra-load-path (list (expand-file-name "tree-sitter"
                                                   user-emacs-local-directory)))
  (treesit-language-source-alist '((python
                                   "https://github.com/tree-sitter/tree-sitter-python"))))

(provide 'cjvmacs-editor)

;;; cjvmacs-editor.el ends here
