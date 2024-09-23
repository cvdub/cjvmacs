(use-package copilot
  :ensure (:fetcher github :repo "zerolfx/copilot.el"
                    :files ("dist" "*.el"))
  :defer t
  ;; :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . #'copilot-accept-completion)
              ("TAB" . #'copilot-accept-completion)
              ("C-<tab>" . #'copilot-accept-completion-by-word)
              ("C-TAB" . #'copilot-accept-completion-by-word)))

(use-package gptel
  :bind (:map cjv/ai-map
              ("g" . #'gptel))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model "gpt-4o-mini"))

(provide 'init-ai)
