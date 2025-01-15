;;; cjvmacs-tools.el --- Tools config for CJVmacs    -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Christian Vanderwall

;; Author: Christian Vanderwall <christian@cvdub.net>
;; Keywords: 

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

;; Tools config for CJVmacs

;;; Code:

(use-package eshell
  :defer t
  :custom
  (eshell-directory-name (expand-file-name "eshell/" user-emacs-cache-directory)))

(use-package tramp
  :defer t
  :custom
  (tramp-persistency-file-name (expand-file-name "tramp" user-emacs-local-directory)))

(provide 'cjvmacs-tools)

;;; cjvmacs-tools.el ends here
