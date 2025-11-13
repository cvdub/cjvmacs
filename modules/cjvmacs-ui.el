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
  :diminish visual-line-mode
  :custom
  (column-number-mode t)
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  :config
  (set-fringe-bitmap-face 'right-curly-arrow 'font-lock-comment-face)
  (set-fringe-bitmap-face 'left-curly-arrow 'font-lock-comment-face))

(use-package custom
  :bind (:map cjv/toggle-map
              ("t" . #'cjv/rotate-theme))
  :custom
  (custom-safe-themes t)
  (custom-theme-directory (expand-file-name "themes" user-emacs-directory))
  :config
  (defun cjv/macos-dark-mode? ()
    "Return t if macOS Appearance is Dark (via AppleScript)."
    (string= (string-trim
              (shell-command-to-string
               "osascript -e 'tell application \"System Events\" to tell appearance preferences to get dark mode'"))
             "true"))

  (defun cjv/update-macos-titlebar (&optional _theme)
    "Make MacOS titlebar transparent on all frames."
    (when (eq system-type 'darwin)
      (dolist (frame (frame-list))
        (modify-frame-parameters
         frame `((mac-transparent-titlebar . t)
                 (ns-appearance . ,(frame-parameter frame 'background-mode)))))))

  (add-hook 'enable-theme-functions #'cjv/update-macos-titlebar)

  (defun cjv/rotate-theme ()
    "Rotate between themes in `cjv/themes'."
    (interactive)
    (let* ((current-theme (car (remove 'cjv-faces custom-enabled-themes)))
           (index (cl-position current-theme cjv/themes))
           (next-theme (or (nth (1+ index) cjv/themes)
                           (car cjv/themes))))
      (disable-theme current-theme)
      (load-theme next-theme)
      (load-theme 'cjv-faces)))

  (if (cjv/macos-dark-mode?)
      (load-theme cjv/theme-dark)
    (load-theme cjv/theme-light)))

(use-package ef-themes
  :ensure t
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
  :init (add-to-list 'load-path (expand-file-name "themes/cjv-faces" user-emacs-directory))
  :custom
  (cjv-faces-fixed-pitch-font-family "Fira Code")
  (cjv-faces-variable-pitch-font-family "iA Writer Quattro V")
  (cjv-faces-emoji-fontset-properties '(:family "Apple Color Emoji" :size 11))
  (cjv-faces-fixed-pitch-font-size 130)
  :config
  (load-theme 'cjv-faces))

(use-package frame
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
  :ensure t
  :defer t
  :hook (text-mode . visual-line-fill-column-mode))

(use-package diminish
  :ensure t)

(use-package face-remap
  :defer t
  :diminish buffer-face-mode
  :bind (:map cjv/toggle-map
              ("v" . #'variable-pitch-mode))
  :hook (buffer-face-mode . (lambda () (when buffer-face-mode
                                         (setq cursor-type 'bar)))))

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
  (tab-bar-show nil))

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
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(org-journal-mode)))

(use-package dashboard
  :ensure t
  :hook (dashboard-mode . (lambda ()
                            (setq-local global-hl-line-mode nil)))
  :custom
  (dashboard-startup-banner (expand-file-name "emacs-logo-gruvbox-dark.svg" user-emacs-local-directory))
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
  :bind ("s-t" . nil))

(use-package pulsar
  :ensure t
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
  :ensure t
  :defer t
  :hook (emacs-lisp-mode lisp-interaction-mode lisp-mode))

(provide 'cjvmacs-ui)

;;; cjvmacs-ui.el ends here
