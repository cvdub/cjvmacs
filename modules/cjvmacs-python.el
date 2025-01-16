;;; cjvmacs-python.el --- Python config for CJVmacs  -*- lexical-binding: t; -*-

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

;; Python config for CJVmacs

;;; Code:

(use-package python
  :hook (python-ts-mode . eglot-ensure)
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :custom
  (python-shell-dedicated 'project))

(use-package pyvenv
  :ensure t
  :custom
  (pyvenv-tracking-mode t))

(use-package flymake-ruff
  :ensure t
  :defer t
  :hook (eglot-managed-mode . flymake-ruff-load))

(use-package python-pytest
  :ensure t
  :defer t
  :bind (:map cjv/code-map
              ("t" . #'python-pytest-dispatch)))

(provide 'cjvmacs-python)

;;; cjvmacs-python.el ends here
