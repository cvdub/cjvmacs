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
  (gptel-include-tool-results t)
  :config
  (defvar cjv/gptel-openrouter
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (auth-source-pick-first-password :host "openrouter.ai")
      :models '(anthropic/claude-opus-4.7
                anthropic/claude-sonnet-4.7
                google/gemini-3.1-flash-lite-preview
                google/gemini-3.1-flash-lite-preview:online
                google/gemini-3-flash-preview
                google/gemini-3-flash-preview:online
                google/gemini-3.1-pro-preview
                google/gemini-3.1-pro-preview:online
                openai/gpt-5.4-mini
                openai/gpt-5.4-mini:online
                openai/gpt-5.4
                openai/gpt-5.4:online
                openai/gpt-5.5
                openai/gpt-5.5:online)))

  (defvar cjv/gptel-ollama
    (gptel-make-ollama "Ollama"
      :host "localhost:11434"
      :stream t
      :models '(llama3.2:3b
                qwen2.5:7b)))

  (defvar cjv/gptel-backends (list cjv/gptel-openrouter
                                   cjv/gptel-ollama))

  (setq gptel-model 'google/gemini-3.1-flash-lite-preview
        gptel-backend cjv/gptel-openrouter)

  (defun cjv/gptel-set-model ()
    (interactive)
    (let* ((all-models (cl-loop for b in cjv/gptel-backends
                                append (gptel-backend-models b)))
           (selected (completing-read "Model: " all-models)))
      (setq gptel-model (intern selected))
      (message "gptel-model set to %s" selected)))

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "** @user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "** @assistant\n"))

(use-package gptel-project
  :defer t
  :load-path "~/code/projects/elisp/gptel-project"
  :bind (:map project-prefix-map
              ("a" . #'gptel-project-chat)
              ("u" . #'gptel-project-update-summary)))

(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :custom
  (claude-code-display-window-fn
   (lambda (buffer)
     (display-buffer buffer '(display-buffer-same-window))))
  :config
  (claude-code-mode)

  (defun cjv/claude-notify (title message)
    "Display a macOS notification with sound."
    ;; (cjv/alert title message)
    (cjv/tab-attention-start (current-buffer)))

  (defun cjv/claude-tab-attention-cleanup ()
    "Clear indicator if current tab has it but no Claude buffer is visible."
    (let* ((tab (assq 'current-tab (funcall tab-bar-tabs-function)))
           (name (alist-get 'name tab)))
      (when (and (string-prefix-p cjv/tab-attention-indicator name)
                 (not (cl-some (lambda (w)
                                 (claude-code--buffer-p (window-buffer w)))
                               (window-list))))
        (tab-bar-rename-tab (substring name (length cjv/tab-attention-indicator))))))
  (add-hook 'buffer-list-update-hook #'cjv/claude-tab-attention-cleanup)

  (setq claude-code-notification-function #'cjv/claude-notify)

  ;; Workaround: claude-code.el fails to set the eat bell handler.
  ;; Detect BEL in process output directly, stripping OSC sequences first.
  ;; Also clear the tab attention indicator on non-bell output (meaning
  ;; Claude is processing, so the user already responded).
  (advice-remove 'eat--filter 'cjv/eat-bell-detect)
  (with-eval-after-load 'eat
    (advice-add 'eat--filter :after
                (defun cjv/eat-bell-detect (process output)
                  (when (and (buffer-live-p (process-buffer process))
                             (claude-code--buffer-p (process-buffer process)))
                    (let ((cleaned (replace-regexp-in-string "\033]0;[^\007]*\007" "" output)))
                      (if (string-match-p "\007" cleaned)
                          (with-current-buffer (process-buffer process)
                            (claude-code--notify nil))
                        (cjv/tab-attention-clear-for-buffer (process-buffer process))))))))

  ;; :bind (("s-c" . claude-code-transient)
  ;;        (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))
  )

(use-package shell-maker
  :vc (:url "https://github.com/xenodium/shell-maker.git" :rev :newest))


(defvar cjv/ai-map2 (make-sparse-keymap)
  "Keymap for my AI commands.")

(global-set-key (kbd "s-c") cjv/ai-map2)


(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell.git" :rev :newest)
  ;; :ensure-system-package
  ;; ((claude-agent-acp . "npm install -g @zed-industries/claude-agent-acp")
  ;;  (codex-agent-acp . "npm install -g @zed-industries/codex-acp"))
  :bind (:map cjv/ai-map2
              ("c" . #'agent-shell)
              :map agent-shell-mode-map
              ("RET" . newline)
              ("C-c C-c" . shell-maker-submit)
              ("C-c C-k" . agent-shell-interrupt))
  :custom
  (agent-shell-show-welcome-message nil)
  (agent-shell-show-usage-at-turn-end t)
  :config
  (defvar cjv/consult-source-agent-shell-buffer
    `(:name "Agent Shell"
            :narrow ?a
            :category buffer
            :face consult-buffer
            :history buffer-name-history
            :state ,#'consult--buffer-state
            :default t
            :items
            ,(lambda ()
               (consult--buffer-query
                :sort 'visibility
                :as #'buffer-name
                :predicate (lambda (buf)
                             (with-current-buffer buf
                               (derived-mode-p 'agent-shell-mode))))))
    "Consult source for agent-shell buffers.")

  (add-to-list 'consult-buffer-sources 'cjv/consult-source-agent-shell-buffer 'append))

(use-package agent-shell-knockknock
  :vc (:url "https://github.com/xenodium/agent-shell-knockknock" :rev :newest)
  :after (agent-shell knockknock)
  :hook (agent-shell-mode . agent-shell-knockknock-mode))

(use-package agent-shell-workspace
  :vc (:url "https://github.com/gveres/agent-shell-workspace")
  :ensure t
  :after agent-shell
  :bind (:map cjv/ai-map2
              ("t" . agent-shell-workspace-toggle)))

;; (use-package agent-shell-sidebar
;;   :after agent-shell
;;   :vc (:url "https://github.com/cmacrae/agent-shell-sidebar")
;;   :bind (:map cjv/ai-map2
;;               ("t" . #'agent-shell-sidebar-toggle))
;;   :custom
;;   (agent-shell-sidebar-default-config
;;    (agent-shell-anthropic-make-claude-code-config)))

(provide 'cjvmacs-ai)

;;; cjvmacs-ai.el ends here
