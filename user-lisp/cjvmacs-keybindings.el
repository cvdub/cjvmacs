;;; cjvmacs-keybindings.el --- Keybinding setup for CJVmacs  -*- lexical-binding: t; -*-

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

;; Keybinding setup for CJVmacs

;;; Code:

;; Fix keybindings on macos
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super))

;; Disable suspend-frame binding
(global-unset-key (kbd "C-z"))

;;;; Keymaps
(defvar cjv/global-keymap (make-keymap)
  "Keymap for cjv/keybindings-mode.")

(define-minor-mode cjv/keybindings-mode
  "Minor mode for custom keybindings."
  :init-value t
  :global t
  :keymap cjv/global-keymap)

;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists
             `((cjv/keybindings-mode . ,cjv/global-keymap)))
(define-key cjv/global-keymap (kbd "M-o") #'other-window)

(defvar cjv/open-map (make-sparse-keymap)
  "Keymap for opening stuff.")

(global-set-key (kbd "C-c o") cjv/open-map)

(defvar cjv/code-map (make-sparse-keymap)
  "Keymap for code related commands.")

(global-set-key (kbd "C-c c") cjv/code-map)

(defvar cjv/toggle-map (make-sparse-keymap)
  "Keymap for toggle commands.")

(global-set-key (kbd "C-c t") cjv/toggle-map)

(defvar cjv/my-map (make-sparse-keymap)
  "Keymap for my personal commands.")

(global-set-key (kbd "C-c m") cjv/my-map)

(defvar cjv/notes-map (make-sparse-keymap)
  "Keymap for notes related commands.")

(global-set-key (kbd "C-c n") cjv/notes-map)

;;;; Shortcuts
(defun cjv/open-downloads ()
  "Opens the downloads directory."
  (interactive)
  (find-file "~/Downloads/"))

(global-set-key (kbd "<f6>") #'cjv/open-downloads)

(defun cjv/open-desktop ()
  "Opens the desktop directory."
  (interactive)
  (find-file "~/Desktop/"))

(global-set-key (kbd "<S-f6>") #'cjv/open-desktop)

(provide 'cjvmacs-keybindings)

;;; cjvmacs-keybindings.el ends here
