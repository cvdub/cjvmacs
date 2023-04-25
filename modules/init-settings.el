;; User info
(setq user-full-name "Christian Vanderwall"
      user-mail-address "christian@cvdub.net")

;; Behavior
(fset 'yes-or-no-p 'y-or-n-p)

;; Expansion
(substitute-key-definition #'dabbrev-expand #'hippie-expand global-map)

;; Revert
(global-set-key (kbd "<f5>") #'revert-buffer)
(global-auto-revert-mode 1)

;; Spaces/Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Ensure environment variables are loaded
(defvar cjv/environment-variables '("PYTHONPATH"
                                    "PYTHONSTARTUP"
                                    "GPG_TTY"
                                    "SSH_AUTH_SOCK"
                                    "ANSIBLE_CONFIG"))

(use-package exec-path-from-shell
  :defer 2
  :init
  (exec-path-from-shell-initialize)
  :config
  (mapc #'exec-path-from-shell-copy-env cjv/environment-variables))

(provide 'init-settings)
