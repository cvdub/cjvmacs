;;;; Basic settings
(setq inhibit-startup-screen t)
(blink-cursor-mode -1)

(when window-system
  (set-scroll-bar-mode nil)
  (tool-bar-mode -1))

;;;; Mode line
(column-number-mode)

;;;; Font
(when window-system
  (set-face-attribute 'default nil
                      :family "Fira Code"
                      :height 160
                      :weight 'light))

;;;; Theme
(use-package gruvbox-theme
  :init
  (load-theme 'gruvbox-light-medium t)
  :config
  (custom-theme-set-faces
   'gruvbox-light-medium
   '(highlight ((t (:background "#ebdbb2" :foreground "#282828")))))
  (enable-theme 'gruvbox-light-medium))

;;;; Which Key
(use-package which-key
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.5))

;;;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode))

(provide 'init-ui)
