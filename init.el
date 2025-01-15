;;; init.el --- Initialization file for CJVmacs -*- lexical-binding: t; -*-

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

;; Init file for cvdub's Emacs config.

;;; Code:

;; Packages
(use-package package
  :config (package-initialize)
  :custom
  (package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
		      ("nongnu" . "https://elpa.nongnu.org/nongnu/")
		      ("melpa" . "https://melpa.org/packages/")))
  (package-user-dir (expand-file-name "packages/" user-emacs-local-directory))
  (package-native-compile t))

;; Utils
(defun set-exec-path-from-shell-PATH ()
  "Set up variable `exec-path' and PATH environment variable to match shell.

This is particularly useful under Mac OS X and macOS, where GUI apps are
not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
					  ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(defun hide-minor-mode (minor-mode)
  "Remove MINOR-MODE from `minor-mode-alist'."
  (setq minor-mode-alist (assq-delete-all minor-mode minor-mode-alist)))

;; Load modules
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))
(require 'cjvmacs-ui)
(require 'cjvmacs-editor)
(require 'cjvmacs-completion)
(require 'cjvmacs-tools)
(require 'cjvmacs-org)
(require 'cjvmacs-email)
(require 'cjvmacs-python)

(setq elisp-flymake-byte-compile-load-path load-path)

;;; init.el ends here






