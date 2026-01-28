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

(cjv-faces-update-faces cjv-faces-face-attributes)

(provide-theme 'cjv-faces)

;;; cjv-faces-theme.el ends here
