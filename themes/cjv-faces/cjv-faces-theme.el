;;; cjv-faces-theme.el --- CJVmacs theme for font sizes and styles -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Christian Vanderwall

;; Author: Christian Vanderwall <christian@cvdub.net>
;; Keywords: faces

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

;; This theme configures font sizes and styles to improve the look of
;; Emacs. It doesn't set any colors and is intended to be used along side
;; another color theme.

;;; Code:

(eval-and-compile
  (require 'cjv-faces))

(deftheme cjv-faces
  "CJVmacs theme for font sizes and styles.")
(put 'cjv-faces 'theme-immediate t)

;;;; Defaults
(cjv-faces-update-faces
 (default :family cjv-faces-fixed-pitch-font-family :height cjv-faces-fixed-pitch-font-size)
 (fixed-pitch :family cjv-faces-fixed-pitch-font-family)
 (variable-pitch :family cjv-faces-variable-pitch-font-family :height cjv-faces-variable-pitch-font-scaling-factor)
 (font-lock-constant-face :weight 'medium)
 (font-lock-keyword-face :weight 'medium)
 (font-lock-builtin-face :weight 'medium))

;;;; Org
(with-eval-after-load 'org  
  (cjv-faces-update-faces
   (org-document-title :height 1.25)
   (org-level-1 :weight 'extra-bold :height 1.2)
   (org-level-2 :weight 'bold :height 1.1)
   (org-todo :inherit 'fixed-pitch)
   (org-document-info-keyword :height 0.9)
   (org-meta-line :height 0.9)
   (org-block-begin-line :height 0.8)
   (org-block-end-line :height 0.8)
   (org-tag :inherit 'fixed-pitch :height 0.8)
   (org-checkbox :inherit 'fixed-pitch)
   (org-checkbox-statistics-todo :inherit 'fixed-pitch :height 0.8 :weight 'bold)
   (org-checkbox-statistics-done :inherit 'fixed-pitch :height 0.8 :weight 'bold)
   (org-drawer :height 0.8)
   (org-special-keyword :height 0.8)
   (org-property-value :inherit 'variable-pitch :height 0.9 :weight 'bold)))

(with-eval-after-load 'message
  (cjv-faces-update-faces
   (message-header-name :height 0.8)
   (message-mml :height 0.9)))

(provide-theme 'cjv-faces)

;;; cjv-faces-theme.el ends here
