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

(defcustom cjv-faces-fixed-pitch-font-size 140
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

(defun cjv-faces-update-face (face &rest attrs)
  "Merge ATTRS with existing attributes on FACE."
  (let* ((existing-attrs (if (facep face)
                             (face-all-attributes face (selected-frame))
                           nil))
         (existing-attrs (seq-remove (lambda (pair)
                                       (or (member (cdr pair) (list 'unspecified nil))
                                           (plist-member attrs (car pair))))
                                     existing-attrs))
         (existing-attrs (flatten-tree existing-attrs)))
    (apply #'custom-theme-set-faces `(cjv-faces (,face ((t ,(append attrs existing-attrs))))))))

(defmacro cjv-faces-update-faces (&rest body)
  "Merge existing face attributes with attributes in BODY."
  (let ((result (mapcar (lambda (row)
                          (let ((face (car row))
                                (attrs (cdr row)))
                            `(cjv-faces-update-face (quote ,face) ,@attrs)))
                        body)))
    `(let ((custom--inhibit-theme-enable nil))
       ,@result)))

;;;; Add themes from package to path

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (file-equal-p dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'cjv-faces)

;;; cjv-faces.el ends here
