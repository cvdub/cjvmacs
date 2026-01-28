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

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
		         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
		         ("melpa" . "https://melpa.org/packages/"))
      package-native-compile t
      package-install-upgrade-built-in t)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package no-littering)

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables '("ANSIBLE_CONFIG"
                                    "INFOPATH"
                                    "MANPATH"
                                    "OBJC_DISABLE_INITIALIZE_FORK_SAFETY"
                                    "PATH"
                                    "PYTHONPATH"
                                    "PYTHONSTARTUP"))
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

;; Load modules
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))
(require 'cjvmacs-utils)
(require 'cjvmacs-keybindings)
(require 'cjvmacs-ui)
(require 'cjvmacs-editor)
(require 'cjvmacs-completion)
(require 'cjvmacs-tools)
(require 'cjvmacs-ai)
(require 'cjvmacs-org)
(require 'cjvmacs-email)
(require 'cjvmacs-python)
(require 'cjvmacs-html)
(require 'cjvmacs-js)
(require 'cjvmacs-lisp)
(require 'cjvmacs-spacebase)
(require 'cjvmacs-rust)
