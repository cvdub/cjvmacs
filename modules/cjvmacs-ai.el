;;; cjvmacs-ai.el --- AI config for CJVmacs -*- lexical-binding: t; -*-

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

;; AI config for CJVmacs

;;; Code:

(defvar cjv/ai-map (make-sparse-keymap)
  "Keymap for my AI commands.")

(global-set-key (kbd "C-c a") cjv/ai-map)

(use-package gptel
  :ensure t
  :defer t
  :bind (:map cjv/ai-map
              ("g" . #'gptel)
              ("a" . #'gptel-add)
              ("s" . #'gptel-send))
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (gptel-make-anthropic "Claude"
    :stream t
    :key (gptel-api-key-from-auth-source "api.anthropic.com"))
  (gptel-make-anthropic "Claude-thinking"
    :key (gptel-api-key-from-auth-source "api.anthropic.com")
    :stream t
    :models '(claude-3-7-sonnet-20250219)
    :header (lambda () (when-let* ((key (gptel--get-api-key)))
                         `(("x-api-key" . ,key)
                           ("anthropic-version" . "2023-06-01")
                           ("anthropic-beta" . "pdfs-2024-09-25")
                           ("anthropic-beta" . "output-128k-2025-02-19")
                           ("anthropic-beta" . "prompt-caching-2024-07-31"))))
    :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
                                :max_tokens 4096)))

(use-package copilot
  :ensure t
  :hook ((prog-mode . copilot-mode)
         (copilot-mode . (lambda () (completion-preview-mode -1))))
  :bind (:map copilot-mode-map
              ("<tab>" . #'copilot-accept-completion)
              ("C-g" . #'copilot-clear-overlay)
              :map cjv/toggle-map
              ("c" . #'copilot-mode))
  :custom
  (copilot-install-dir (expand-file-name "copilot" user-emacs-cache-directory))
  (copilot-indent-offset-warning-disable t)
  (copilot-max-char-warning-disable t))

(provide 'cjvmacs-ai)

;;; cjvmacs-ai.el ends here
