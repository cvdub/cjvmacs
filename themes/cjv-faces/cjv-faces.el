;;; cjv-faces.el --- Package for cjv-faces theme     -*- lexical-binding: t; -*-

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

;; Package for cjv-faces theme.

;;; Code:

(require 'cl-lib)

(defgroup cjv-faces ()
  "User options for cjv-faces."
  :group 'faces
  :prefix "cjv-faces-"
  :tag "CJV faces")

(defcustom cjv-faces-fixed-pitch-font-family "Monospace"
  "Fixed pitch font family for cjv-faces."
  :group 'cjv-faces
  :type '(string))

(defcustom cjv-faces-fixed-pitch-font-size 120
  "Fixed pitch font size for cjv-faces."
  :group 'cjv-faces
  :type '(integer))

(defcustom cjv-faces-variable-pitch-font-family "Sans Serif"
  "Variable pitch font family for cjv-faces."
  :group 'cjv-faces
  :type '(string))

(defcustom cjv-faces-variable-pitch-font-scaling-factor 1.0
  "Variable pitch font scaling factor for cjv-faces."
  :group 'cjv-faces
  :type '(float))

(defcustom cjv-faces-face-attributes
  '((default :family cjv-faces-fixed-pitch-font-family
             :height cjv-faces-fixed-pitch-font-size)
    (fixed-pitch :family cjv-faces-fixed-pitch-font-family)
    (variable-pitch :family cjv-faces-variable-pitch-font-family
                    :height cjv-faces-variable-pitch-font-scaling-factor)
    (font-lock-constant-face :weight medium)
    (font-lock-keyword-face :weight medium)
    (font-lock-builtin-face :weight medium)

    ;; Org
    (org-document-title :height 1.15)
    (org-level-1 :weight extra-bold
                 :height 1.3)
    (org-level-2 :weight bold
                 :height 1.15)
    (org-todo :inherit fixed-pitch :weight bold)
    (org-done :inherit fixed-pitch :weight bold)
    (org-document-info-keyword :height 0.9)
    (org-meta-line :height 0.9)
    (org-block-begin-line :inherit fixed-pitch
                          :height 0.8
                          :background unspecified
                          :underline t)
    (org-block-end-line :inherit fixed-pitch
                        :height 0.8
                        :background unspecified
                        :overline t)
    (org-tag :inherit fixed-pitch
             :height 0.8
             :weight bold)
    (org-checkbox :inherit fixed-pitch)
    (org-checkbox-statistics-todo :inherit fixed-pitch
                                  :height 0.8
                                  :weight bold)
    (org-checkbox-statistics-done :inherit fixed-pitch
                                  :height 0.8
                                  :weight bold)
    (org-drawer :height 0.8)
    (org-special-keyword :height 0.8)
    (org-property-value :inherit variable-pitch
                        :height 0.9
                        :weight bold)
    (org-table :inherit fixed-pitch)

    ;; Message
    (message-header-name :height 0.8
                         :weight semibold
                         :inherit fixed-pitch)
    (message-mml :height 0.8
                 :weight semibold
                 :inherit fixed-pitch))
  "List of faces and attributes to remap."
  :group 'cjv-faces
  :type '(alist :key-type symbol :value-type (list)))

(defcustom cjv-faces-emoji-fontset-properties nil
  "Fontset properties to use for emojis.

If nil, the default emoji fontset is not altered."
  :group 'cjv-faces
  :type '(plist))

(defvar cjv-faces--default-emoji-fontset-properties nil
  "Cached value for the default emoji fontset properties.")

(defconst cjv-faces--fontset-properties '(:name
                                          :family
                                          :foundry
                                          :weight
                                          :slant
                                          :width
                                          :size
                                          :adstyle
                                          :registry
                                          :dpi
                                          :spacing
                                          :avgwidth
                                          :script
                                          :lang
                                          :otf
                                          :type))

(defun cjv-faces--update-emoji-fontset-properties (property-list)
  "Update the emoji fontset according to PROPERTY-LIST.

The default values are saved and restored when this theme is disabled."
  (cl-loop with font = (font-at 0 nil "😎")
           for property in cjv-faces--fontset-properties
           for value = (font-get font property)
           when (and value (or (not (listp value)) (car value)))
           append (list property value) into font-spec-properties
           finally
           (unless cjv-faces--default-emoji-fontset-properties
             (setq cjv-faces--default-emoji-fontset-properties font-spec-properties))
           (set-fontset-font
            t 'emoji
            (apply #'font-spec (append font-spec-properties property-list)))))

(advice-add 'enable-theme :after
            (lambda (theme)
              (when (and (eq theme 'cjv-faces)
                         cjv-faces-emoji-fontset-properties)
                (cjv-faces--update-emoji-fontset-properties
                 cjv-faces-emoji-fontset-properties))))

(advice-add 'disable-theme :after
            (lambda (theme)
              (when (and (eq theme 'cjv-faces)
                         cjv-faces--default-emoji-fontset-properties)
                (cjv-faces--update-emoji-fontset-properties
                 cjv-faces--default-emoji-fontset-properties))))

(defun cjv-faces--symbol-or-value (v)
  "Evaluate V if its a custom variable, otherwise return as is."
  (if (custom-variable-p v)
      (eval v)
    v))

(defun cjv-faces-update-faces (face-attributes)
  "Update face attributes according to FACE-ATTRIBUTES."
  (cl-loop for (face . attrs) in face-attributes
           for attrs = (mapcar #'cjv-faces--symbol-or-value attrs)
           do (apply #'custom-theme-set-faces `(cjv-faces (,face ((t ,@attrs)))))))

;;;; Add themes from package to path

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (file-equal-p dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'cjv-faces)

;;; cjv-faces.el ends here
