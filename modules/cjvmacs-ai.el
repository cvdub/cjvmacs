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
              ("s" . #'gptel-send)
              ("r" . #'gptel-rewrite))
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (defvar cjv/gptel-backend (gptel-make-openai "OpenRouter"
                              :host "openrouter.ai"
                              :endpoint "/api/v1/chat/completions"
                              :stream t
                              :key (auth-source-pick-first-password :host "openrouter.ai")
                              :models '((anthropic/claude-3.5-sonnet
                                         :description "New Claude 3.5 Sonnet delivers better-than-Opus capabilities, faster-than-Sonnet speeds, at the same Sonnet prices. Sonnet is particularly good at:"
                                         :capabilities (media tool-use json url)
                                         :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                                         :context-window 200
                                         :input-cost 3
                                         :output-cost 15
                                         :cutoff-date "2024-10")
                                        (anthropic/claude-3.7-sonnet
                                         :description "Claude 3.7 Sonnet is an advanced large language model with improved reasoning, coding, and problem-solving capabilities. It introduces a hybrid reasoning approach, allowing users to choose between rapid responses and extended, step-by-step processing for complex tasks. The model demonstrates notable improvements in coding, particularly in front-end development and full-stack updates, and excels in agentic workflows, where it can autonomously navigate multi-step processes. "
                                         :capabilities (media tool-use json url)
                                         :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                                         :context-window 200
                                         :input-cost 3
                                         :output-cost 15
                                         :cutoff-date "2025-02")
                                        (anthropic/claude-3.7-sonnet:thinking
                                         :description "Claude 3.7 Sonnet is an advanced large language model with improved reasoning, coding, and problem-solving capabilities. It introduces a hybrid reasoning approach, allowing users to choose between rapid responses and extended, step-by-step processing for complex tasks. The model demonstrates notable improvements in coding, particularly in front-end development and full-stack updates, and excels in agentic workflows, where it can autonomously navigate multi-step processes. "
                                         :capabilities (media tool-use json url)
                                         :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                                         :context-window 200
                                         :input-cost 3
                                         :output-cost 15
                                         :cutoff-date "2025-02")
                                        (google/gemini-2.0-flash-001
                                         :description "Gemini Flash 2.0 offers a significantly faster time to first token (TTFT) compared to [Gemini Flash 1.5](/google/gemini-flash-1.5), while maintaining quality on par with larger models like [Gemini Pro 1.5](/google/gemini-pro-1.5). It introduces notable enhancements in multimodal understanding, coding capabilities, complex instruction following, and function calling. These advancements come together to deliver more seamless and robust agentic experiences."
                                         :capabilities (media tool-use json url)
                                         :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                                         :context-window 1000
                                         :input-cost 0.09999999999999999
                                         :output-cost 0.39999999999999997
                                         :cutoff-date "2025-02")
                                        (google/gemini-2.5-pro-preview-03-25
                                         :description "Gemini 2.5 Pro is Google’s state-of-the-art AI model designed for advanced reasoning, coding, mathematics, and scientific tasks. It employs “thinking” capabilities, enabling it to reason through responses with enhanced accuracy and nuanced context handling. Gemini 2.5 Pro achieves top-tier performance on multiple benchmarks, including first-place positioning on the LMArena leaderboard, reflecting superior human-preference alignment and complex problem-solving abilities."
                                         :capabilities (media tool-use json url)
                                         :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                                         :context-window 1000
                                         :input-cost 1.25
                                         :output-cost 10
                                         :cutoff-date "2025-04")
                                        (openai/gpt-4o
                                         :description "GPT-4o (o for omni) is OpenAI's latest AI model, supporting both text and image inputs with text outputs. It maintains the intelligence level of [GPT-4 Turbo](/models/openai/gpt-4-turbo) while being twice as fast and 50% more cost-effective. GPT-4o also offers improved performance in processing non-English languages and enhanced visual capabilities."
                                         :capabilities (media tool-use json url)
                                         :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                                         :context-window 128
                                         :input-cost 2.5
                                         :output-cost 10
                                         :cutoff-date "2024-05")
                                        (openai/gpt-4o-mini
                                         :description "GPT-4o mini is OpenAI's newest model after [GPT-4 Omni](/models/openai/gpt-4o), supporting both text and image inputs with text outputs."
                                         :capabilities (media tool-use json url)
                                         :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                                         :context-window 128
                                         :input-cost 0.15
                                         :output-cost 0.6
                                         :cutoff-date "2024-07")
                                        (openai/o1
                                         :description "The latest and strongest model family from OpenAI, o1 is designed to spend more time thinking before responding. The o1 model series is trained with large-scale reinforcement learning to reason using chain of thought."
                                         :capabilities (media tool-use json url)
                                         :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                                         :context-window 200
                                         :input-cost 15
                                         :output-cost 60
                                         :cutoff-date "2024-12")
                                        (openai/o1-mini
                                         :description "The latest and strongest model family from OpenAI, o1 is designed to spend more time thinking before responding."
                                         :capabilities (media tool-use json url)
                                         :mime-types ()
                                         :context-window 128
                                         :input-cost 1.1
                                         :output-cost 4.4
                                         :cutoff-date "2024-09")
                                        (openai/o1-pro
                                         :description "The o1 series of models are trained with reinforcement learning to think before they answer and perform complex reasoning. The o1-pro model uses more compute to think harder and provide consistently better answers."
                                         :capabilities (media tool-use json url)
                                         :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                                         :context-window 200
                                         :input-cost 150
                                         :output-cost 600
                                         :cutoff-date "2025-03")
                                        (openai/o3-mini
                                         :description "OpenAI o3-mini is a cost-efficient language model optimized for STEM reasoning tasks, particularly excelling in science, mathematics, and coding."
                                         :capabilities (media tool-use json url)
                                         :mime-types ()
                                         :context-window 200
                                         :input-cost 1.1
                                         :output-cost 4.4
                                         :cutoff-date "2025-01")
                                        (openai/o3-mini-high
                                         :description "OpenAI o3-mini-high is the same model as o3-mini with reasoning_effort set to high."
                                         :capabilities (media tool-use json url)
                                         :mime-types ()
                                         :context-window 200
                                         :input-cost 1.1
                                         :output-cost 4.4
                                         :cutoff-date "2025-02"))))
  (setq gptel-backend cjv/gptel-backend
        gptel-model 'anthropic/claude-3.5-sonnet)
  (setq gptel--known-backends (assoc-delete-all "ChatGPT" gptel--known-backends #'string-prefix-p)))

(use-package copilot
  :ensure t
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

(provide 'cjvmacs-ai)

;;; cjvmacs-ai.el ends here
