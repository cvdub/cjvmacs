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

(defvar cjv/themes '(gruvbox-dark-medium
                     gruvbox-light-medium))
(defvar cjv/theme-light 'gruvbox-light-medium)
(defvar cjv/theme-dark 'gruvbox-dark-medium)

(use-package emacs
  :bind ("C-+" . #'global-text-scale-adjust)
  :custom
  (tool-bar-mode nil)
  (ring-bell-function 'ignore)
  (use-short-answers t)
  (use-dialog-box nil)
  (fill-column 100)
  (frame-resize-pixelwise t)
  :config
  (modify-all-frames-parameters  '((border-width . 0)
                                   (internal-border-width . 0))))

(use-package simple
  :ensure nil
  :diminish visual-line-mode
  :custom
  (column-number-mode t)
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  :config
  (set-fringe-bitmap-face 'right-curly-arrow 'font-lock-comment-face)
  (set-fringe-bitmap-face 'left-curly-arrow 'font-lock-comment-face))

(use-package custom
  :ensure nil
  :bind (:map cjv/toggle-map
              ("t" . #'cjv/rotate-theme))
  :custom
  (custom-safe-themes t)
  (custom-theme-directory (expand-file-name "themes" no-littering-etc-directory))
  :config

  (defun cjv/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (let ((theme (pcase appearance
                   ('light cjv/theme-light)
                   ('dark cjv/theme-dark))))
      (cjv/load-theme theme)
      (cjv/sync-claude-code-theme appearance)))

  (defun cjv/load-theme (theme)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (load-theme 'cjv-faces)
    (cjv/set-theme-color-vars))

  (defun cjv/sync-claude-code-theme (appearance)
    "Update Claude Code theme in ~/.claude.json to match APPEARANCE."
    (let ((config-file (expand-file-name "~/.claude.json"))
          (theme (pcase appearance
                   ('light "light-daltonized")
                   ('dark "dark-daltonized"))))
      (when (and theme (file-exists-p config-file))
        (with-temp-buffer
          (insert-file-contents config-file)
          (when (re-search-forward "\"theme\"\\s-*:\\s-*\"[^\"]*\"" nil t)
            (replace-match (format "\"theme\": \"%s\"" theme)))
          (write-region (point-min) (point-max) config-file nil 'silent)))))

  (add-hook 'ns-system-appearance-change-functions #'cjv/apply-theme)

  (defun cjv/rotate-theme ()
    "Rotate between themes in `cjv/themes'."
    (interactive)
    (let* ((current-theme (car (remove 'cjv-faces custom-enabled-themes)))
           (index (cl-position current-theme cjv/themes))
           (next-theme (or (nth (1+ index) cjv/themes)
                           (car cjv/themes))))
      (cjv/load-theme next-theme))))

(use-package ef-themes
  :defer t
  :custom
  (ef-cyprus-palette-overrides '((bg-mode-line bg-active)
                                 (prose-done fg-dim)
                                 (mail-recipient blue-faint)
                                 (bg-region bg-cyan-subtle)))
  :config
  (with-eval-after-load 'org
    (ef-themes-with-colors
     (set-face-attribute 'org-todo-done nil :foreground green-faint)
     (set-face-attribute 'org-todo-someday nil :foreground border)
     (set-face-attribute 'org-checkbox-statistics-todo nil :foreground fg-dim))))

(use-package gruvbox-theme
  :vc (:url "git@github.com:cvdub/emacs-theme-gruvbox.git" :rev :newest))

(use-package cjv-faces
  :ensure nil
  :init (add-to-list 'load-path (expand-file-name "cjv-faces" custom-theme-directory))
  :custom
  (cjv-faces-fixed-pitch-font-family "Fira Code")
  (cjv-faces-variable-pitch-font-family "iA Writer Quattro V")
  (cjv-faces-emoji-fontset-properties '(:family "Apple Color Emoji" :size 11))
  (cjv-faces-fixed-pitch-font-size 130)
  :config
  (load-theme 'cjv-faces))

(use-package frame
  :ensure nil
  :custom
  (blink-cursor-mode nil))

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil))

(use-package hl-line
  :bind (:map cjv/toggle-map
              ("h" . #'hl-line-mode))
  :hook ((prog-mode . hl-line-mode)
         (org-agenda-mode . hl-line-mode)))

(use-package visual-fill-column
  :defer t
  :hook (visual-line-mode-hook . visual-fill-column-for-vline))

(use-package diminish)

(use-package face-remap
  :defer t
  :diminish buffer-face-mode
  :bind (:map cjv/toggle-map
              ("v" . #'variable-pitch-mode))
  :hook (buffer-face-mode . (lambda () (when buffer-face-mode
                                         (setq cursor-type 'bar)))))

(use-package mixed-pitch
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
  :defer t
  ;; :hook (writeroom-mode . cjv/writeroom-increase-text-scaling)
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
  (writeroom-fullscreen-effect 'maximized)
  (writeroom-width 160))

(use-package tab-bar
  :init (tab-bar-mode 1)
  :bind (:map tab-bar-map
              ("C-<tab>" . #'tab-next)
              :map cjv/toggle-map
              ("b" . #'cjv/toggle-tab-bar))
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-show 1)
  (tab-bar-auto-width nil)
  (tab-bar-auto-width-max '((220) 20))
  :config
  (defun cjv/toggle-tab-bar ()
    (interactive)
    (if tab-bar-show
        (setq tab-bar-show nil)
      (setq tab-bar-show 1))
    (tab-bar-mode 1))

  (defun cjv/tab-bar-name-add-space (name tab _i)
    (format " %s " name))
  (add-to-list 'tab-bar-tab-name-format-functions #'cjv/tab-bar-name-add-space))

(use-package diff-hl
  :defer 5
  :hook (dired-mode . diff-hl-dired-mode-unless-remote)
  :config
  (global-diff-hl-mode)
  :custom
  (diff-hl-disable-on-remote))

(use-package rainbow-mode
  :defer t
  :custom
  (rainbow-html-colors nil)
  (rainbow-x-colors nil))

(use-package kurecolor
  :defer t)

(use-package fontify-face
  :vc (:url "https://github.com/Fuco1/fontify-face.git"
            :rev :newest))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(org-journal-mode)))

(use-package dashboard
  :hook (dashboard-mode . (lambda ()
                            (setq-local global-hl-line-mode nil)))
  :custom
  (dashboard-startup-banner (expand-file-name "emacs-logo-gruvbox-dark.svg" no-littering-etc-directory))
  (dashboard-banner-logo-title nil)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-items nil)
  (dashboard-footer-messages '("The one true editor, Emacs!"
                               "Free as free speech, free as free Beer"
                               "Happy coding!"
                               "Welcome to the church of Emacs"
                               "While any text editor can save your files, only Emacs can save your soul"))
  (dashboard-hide-cursor t)
  :custom-face
  (dashboard-footer-face ((t (:foreground "#83a598"))))
  :config

  ;; Redefine dashboard-resize-on-hook so it forces refresh on resize
  (defun dashboard-resize-on-hook (&optional _)
    "Re-render dashboard on window size change."
    (let ((space-win (get-buffer-window dashboard-buffer-name))
          (frame-win (frame-selected-window)))
      (when (and space-win
                 (not (window-minibuffer-p frame-win)))
        (with-selected-window space-win
          (dashboard-insert-startupify-lists t)))))

  (dashboard-setup-startup-hook))

(use-package menu-bar
  :ensure nil
  :bind ("s-t" . nil))

(use-package pulsar
  :init
  (pulsar-global-mode 1)
  :custom
  (pulsar-pulse-on-window-change t))

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll") ; if desired (emacs>=v30)
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

(use-package rainbow-delimiters
  :defer t
  :hook (emacs-lisp-mode lisp-interaction-mode lisp-mode))

(use-package indent-bars
  :defer t
  :hook (yaml-ts-mode . indent-bars-mode)
  :bind (:map cjv/toggle-map
              ("i" . #'indent-bars-mode))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-prefer-character t))

(use-package knockknock
  :vc (:url "https://github.com/konrad1977/knockknock" :rev :newest)
  :custom
  (knockknock-poshandler #'posframe-poshandler-frame-top-center)
  :config
  (defun cjv/knocknock-update-colors (&optional theme)
    (setq knockknock-border-color (face-attribute 'vertical-border :foreground)
          knockknock-background-color (face-background 'default nil t)
          knockknock-foreground-color (face-foreground 'default nil t))
    (clrhash knockknock--svg-cache))
  (cjv/knocknock-update-colors)
  (add-hook 'enable-theme-functions #'cjv/knocknock-update-colors))

(provide 'cjvmacs-ui)

;;; cjvmacs-ui.el ends here
