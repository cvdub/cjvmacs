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
  :vc (:url "git@github.com:cvdub/gptel.git"
            :rev :newest)
  :defer t
  :bind (:map cjv/ai-map
              ("g" . #'gptel)
              ("a" . #'gptel-add)
              ("s" . #'gptel-send)
              ("r" . #'gptel-rewrite)
              ("m" . #'cjv/gptel-set-model)
              ("c" . #'gptel-context-remove-all))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-include-reasoning nil)
  :config
  (setq gptel-api-key (auth-source-pick-first-password :host "openrouter.ai")
        gptel-model 'openai/gpt-4o-mini
        gptel-backend (gptel-make-openai "OpenRouter"
                        :host "openrouter.ai"
                        :endpoint "/api/v1/chat/completions"
                        :stream nil
                        :key (auth-source-pick-first-password :host "openrouter.ai")
                        :models '(openai/gpt-4o-mini
                                  openai/gpt-4o-mini:online
                                  openai/o3
                                  openai/o3:online
                                  openai/o4-mini
                                  openai/o4-mini:online
                                  google/gemini-2.5-pro-preview-03-25
                                  google/gemini-2.5-pro-preview-03-25:online))
        ;; Delete default OpenAI backend
        gptel--known-backends (assoc-delete-all "ChatGPT" gptel--known-backends #'string-prefix-p))

  (defun cjv/gptel-set-model ()
    (interactive)
    (setq gptel-model (intern (completing-read "Model:" (gptel-backend-models gptel-backend))))))

(use-package copilot
  :ensure t
  :defer t
  :hook (;; (prog-mode . copilot-mode)
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

(use-package aidermacs
  :ensure t
  :defer t
  :vc (:url "https://github.com/MatthewZMD/aidermacs.git"
            :rev :newest)
  :bind ("s-a" . #'aidermacs-transient-menu)
  :custom
  (aidermacs-extra-args `("--api-key" ,(format "openrouter=%s" (auth-source-pick-first-password :host "openrouter.ai"))))
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "openrouter/openai/gpt-4o-mini"))

(use-package emigo
  :ensure t
  :defer t
  :vc (:url "https://github.com/MatthewZMD/emigo.git"
            :rev :newest)
  :config
  (emigo-enable) ;; Starts the background process automatically
  :custom
  ;; Encourage using OpenRouter with Deepseek
  (emigo-python-command "/Users/cjv/.config/emacs/.local/packages/emigo/.venv/bin/python")
  (emigo-model "openrouter/deepseek/deepseek-chat-v3-0324")
  (emigo-base-url "https://openrouter.ai/api/v1")
  (emigo-api-key (auth-source-pick-first-password :host "openrouter.ai")))

(provide 'cjvmacs-ai)

;;; cjvmacs-ai.el ends here
