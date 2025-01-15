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
  :custom
  (tool-bar-mode nil)
  (ring-bell-function 'silent)
  (use-short-answers t)
  (use-dialog-box nil))

(use-package faces
  :config
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "Fira Code" :height 140)
    (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))
    (set-face-attribute 'variable-pitch nil :family "iA Writer Quattro V" :height 1.15)
    (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji" :size 11))))

(use-package modus-themes
  :ensure t
  :after faces
  :config
  (modus-themes-load-theme 'modus-vivendi)
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t))

(use-package custom
  :custom
  (custom-enabled-themes '(modus-vivendi)))

(use-package frame
  :custom
  (blink-cursor-mode nil))

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil))

(use-package hl-line
  :custom
  (global-hl-line-mode t))

(provide 'cjvmacs-ui)

;;; cjvmacs-ui.el ends here
