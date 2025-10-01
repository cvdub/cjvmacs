;;; cjvmacs-js.el --- JS config for CJVmacs          -*- lexical-binding: t; -*-

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

;; JS config for CJVmacs

;;; Code:

(use-package typescript-ts-mode
  :defer t
  :mode ("\\.ts[x]?\\'")
  :hook (typescript-ts-mode . eglot-ensure)
  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package js-mode
  :defer t
  :init
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
  :custom
  (js-indent-level 2))

(use-package js-ts-mode
  :defer t
  :mode "\\.js[x]?\\'")

(use-package json-ts-mode
  :defer t
  :mode ("\\.json[c]?\\'"))

(provide 'cjvmacs-js)

;;; cjvmacs-js.el ends here
