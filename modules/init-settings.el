;; User info
(setq user-full-name "Christian Vanderwall"
      user-mail-address "christian@cvdub.net")

;; Behavior
(fset 'yes-or-no-p 'y-or-n-p)

;; Expansion
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Revert
(global-set-key (kbd "<f5>") #'revert-buffer)
(global-auto-revert-mode 1)

;; Spaces/Tabs
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Other window
(global-set-key (kbd "M-o") #'other-window)

;; Disable suspend-frame binding
(global-unset-key (kbd "C-z"))

;; Ensure environment variables are loaded
(defvar cjv/environment-variables '("PYTHONPATH"
                                    "PYTHONSTARTUP"
                                    "GPG_TTY"
                                    "SSH_AUTH_SOCK"
                                    "ANSIBLE_CONFIG"))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  :config
  (mapc #'exec-path-from-shell-copy-env cjv/environment-variables))

(provide 'init-settings)
