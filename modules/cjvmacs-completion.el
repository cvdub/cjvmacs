;;; cjvmacs-completion.el --- Completion config for CJVmacs  -*- lexical-binding: t; -*-

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

;; Completion config for CJVmacs

;;; Code:

(use-package which-key
  :custom
  (which-key-mode t)
  (which-key-compute-remaps t)
  (which-key-idle-delay 0.75)
  (which-key-lighter nil))

(use-package icomplete
  :custom
  (fido-vertical-mode t))

(use-package scroll-bar
  :custom
  (scroll-bar-mode nil))

(use-package minibuffer
  :custom
  ;; (completion-styles '(flex basic))
  ;; (completion-category-overrides '((file (styles basic partial-completion))))
  (completions-detailed nil)
  (completions-format 'one-column)
  (completions-max-height 20)
  (completions-sort 'historical))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package simple
  :custom
  (completion-auto-select t)
  (completion-show-help nil))

(use-package completion-preview
  :diminish completion-preview-mode
  :bind (:map completion-preview-active-mode-map
	      ("M-n" . #'completion-preview-next-candidate)
	      ("M-p". #'completion-preview-prev-candidate))
  :custom
  (completion-preview-message-format "%i/%n possible completions")
  (global-completion-preview-mode t)
  (completion-preview-idle-delay 0.2))

(use-package savehist
  :init
  (setq savehist-file (expand-file-name "history" user-emacs-cache-directory))
  :custom
  (savehist-mode t)
  (savehist-autosave-interval 60)
  (savehist-additional-variables '(search-ring regexp-search-ring)))

(use-package recentf
  :init
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-cache-directory))
  :custom
  (recentf-mode t)
  (recentf-max-saved-items 500)
  (recentf-auto-cleanup 'never))

(provide 'cjvmacs-completion)

;;; cjvmacs-completion.el ends here
