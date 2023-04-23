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
(with-eval-after-load 'org
  (custom-declare-face '+org-todo-todo '((t (:inherit (org-todo)))) "")
  (custom-declare-face '+org-todo-done '((t (:inherit (org-done)))) "")
  (custom-declare-face '+org-todo-onhold '((t (:inherit (font-lock-constant-face org-todo)))) "")
  (custom-declare-face '+org-todo-someday '((t (:inherit (font-lock-comment-face org-todo)))) ""))

(defvar cjv/light-theme 'gruvbox-light-medium)
(defvar cjv/dark-theme 'gruvbox-dark-medium)

(defun cjv/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (let ((theme (pcase appearance
                 ('light cjv/light-theme)
                 ('dark cjv/dark-theme))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (enable-theme theme)))

(use-package gruvbox-theme
  :init
  (load-theme 'gruvbox-light-medium t t)
  :config
  (custom-theme-set-faces
   'gruvbox-light-medium
   '(highlight ((t (:background "#ebdbb2" :foreground "#282828"))))))


;; (cjv/apply-theme 'dark)

;; Switch theme when system dark/light mode changes
(add-hook 'ns-system-appearance-change-functions #'cjv/apply-theme)


;;;; Diminish
(use-package diminish
  :diminish narrow)

;;;; Which Key
(use-package which-key
  :diminish
  :init
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.5))

;;;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode))

(provide 'init-ui)
