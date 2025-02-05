;;; cjvmacs-ui.el --- UI config for CJVmacs -*- lexical-binding: t; -*-

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

;; UI config for CJVmacs

;;; Code:

(use-package emacs
  :bind ("C-+" . #'global-text-scale-adjust)
  :custom
  (tool-bar-mode nil)
  (ring-bell-function 'ignore)
  (use-short-answers t)
  (use-dialog-box nil)
  (fill-column 100))

(use-package simple
  :diminish visual-line-mode
  :custom
  (column-number-mode t))

(use-package custom
  :custom
  (custom-safe-themes t)
  (custom-theme-directory (expand-file-name "themes" user-emacs-directory))
  :config
  (defun cjv/update-macos-titlebar (&optional _theme)
    "Make MacOS titlebar transparent on all frames."
    (when (eq system-type 'darwin)
      (dolist (frame (frame-list))
        (modify-frame-parameters
         frame `((ns-transparent-titlebar . t)
                 (ns-appearance . ,(frame-parameter frame 'background-mode)))))))

  (add-hook 'enable-theme-functions #'cjv/update-macos-titlebar))

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-cyprus)
  (with-eval-after-load 'org
    (ef-themes-with-colors
      (set-face-attribute 'org-todo-done nil :foreground green-faint)
      (set-face-attribute 'org-todo-someday nil :foreground border)
      (set-face-attribute 'org-checkbox-statistics-todo nil :foreground fg-dim)))
  :custom
  (ef-cyprus-palette-overrides '((bg-mode-line bg-active)
                                 (prose-done fg-dim)
                                 (mail-recipient blue-faint)
                                 (bg-region bg-cyan-subtle))))

(use-package cjv-faces
  :init (add-to-list 'load-path (expand-file-name "themes/cjv-faces" user-emacs-directory))
  :custom
  (cjv-faces-fixed-pitch-font-family "Fira Code")
  (cjv-faces-variable-pitch-font-family "iA Writer Quattro V")
  (cjv-faces-emoji-fontset-properties '(:family "Apple Color Emoji" :size 11))
  :config
  (load-theme 'cjv-faces)
  ;; (add-hook 'enable-theme-functions (lambda (theme)
  ;;                                     (unless (eq theme 'cjv-faces)
  ;;                                       (message theme)
  ;;                                       (enable-theme 'cjv-faces))))
  )

(use-package frame
  :custom
  (blink-cursor-mode nil))

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil))

(use-package hl-line
  :bind (:map cjv/toggle-map
              ("h" . #'hl-line-mode))
  :custom
  (global-hl-line-mode t))

(use-package visual-fill-column
  :ensure t
  :defer t
  :hook (text-mode . visual-line-fill-column-mode))

(use-package diminish
  :ensure t)

(use-package face-remap
  :defer t
  :diminish buffer-face-mode
  :bind (:map cjv/toggle-map
              ("v" . #'variable-pitch-mode)))

(use-package mixed-pitch
  :ensure t
  :defer t
  ;; :hook (Info-mode . mixed-pitch-mode)
  :config
  (dolist (face '(notmuch-tag-face
                  notmuch-show-part-button-type-button
                  message-mml))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face))
  (dolist (face '(message-header-subject
                  message-header-to
                  message-header-other))
    (setq mixed-pitch-fixed-pitch-faces (remove face mixed-pitch-fixed-pitch-faces))))

(use-package writeroom-mode
  :ensure t
  :defer t
  :hook (writeroom-mode . cjv/writeroom-increase-text-scaling)
  :bind (:map cjv/toggle-map
              ("w" . #'writeroom-mode))
  :config
  (defun cjv/writeroom-increase-text-scaling ()
    (if writeroom-mode
        (text-scale-adjust 1)
      (text-scale-adjust 0))
    (visual-fill-column-adjust))
  :custom
  (writeroom-mode-line t)
  (writeroom-maximize-window nil)
  (writeroom-fullscreen-effect 'maximized))

(use-package tab-bar
  :init (tab-bar-mode 1)
  :bind (:map tab-bar-map
              ("C-<tab>" . #'tab-next))
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-show 1))

(use-package diff-hl
  :ensure t
  :defer 5
  :hook (dired-mode . diff-hl-dired-mode-unless-remote)
  :config
  (global-diff-hl-mode)
  :custom
  (diff-hl-disable-on-remote))

(use-package rainbow-mode
  :ensure t
  :defer t
  :custom
  (rainbow-html-colors nil)
  (rainbow-x-colors nil))

(use-package kurecolor
  :ensure t
  :defer t)

(use-package fontify-face
  :vc (:url "https://github.com/Fuco1/fontify-face.git"
            :rev :newest))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(provide 'cjvmacs-ui)

;;; cjvmacs-ui.el ends here
