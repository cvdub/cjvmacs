;;;; Basic settings
(setq inhibit-startup-screen t)
(blink-cursor-mode -1)

(when window-system
  (set-scroll-bar-mode nil)
  (tool-bar-mode -1))

(winner-mode 1)

;;;; Mode line
(column-number-mode)

;;;; Font
(when window-system
  (set-face-attribute 'default nil
                      :family "Fira Code"
                      :height 160
                      :weight 'light))

(custom-set-faces
 '(fixed-pitch ((t (:family "Fira Code" :height 160 :weight light))))
 '(variable-pitch ((t (:family "iA Writer Quattro S" :height 180)))))

(custom-set-faces
 '(org-document-title ((t (:height 1.3))))
 '(org-level-1 ((t (:inherit outline-1 :weight extra-bold :height 1.25))))
 '(org-level-2 ((t (:inherit outline-2 :weight bold :height 1.1))))
 '(org-tag ((t (:height .8))))
 '(org-document-info-keyword ((t (:height .9))))
 '(org-meta-line ((t (:height .9))))
 '(org-special-keyword ((t (:height .9))))
 '(org-date ((t (:inherit font-lock-comment-face :height .9))))
 '(org-drawer ((t (:height .9))))
 '(org-block-begin-line ((t (:height .9))))
 '(org-block-end-line ((t (:height .9))))
 '(org-quote ((t (:inherit variable-pitch :slant normal :family "iA Writer Quattro S")))))

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

;; Switch theme when system dark/light mode changes
(add-hook 'ns-system-appearance-change-functions #'cjv/apply-theme)

;;;; Diminish
(use-package diminish
  :diminish (narrow buffer-face-mode))

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

;;;; Visual lines
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
(use-package visual-fill-column
  :defer t
  :hook visual-line-mode)

;;;; Write room
(use-package writeroom-mode
  :defer t
  :hook (writeroom-mode . cjv/writeroom-increase-text-scaling)
  :config
  (defun cjv/writeroom-increase-text-scaling ()
    (text-scale-adjust 1)
    (visual-fill-column-adjust))
  :custom
  (writeroom-mode-line t)
  (writeroom-fullscreen-effect 'maximized))

;;;; Icons
(use-package all-the-icons)
(use-package all-the-icons-dired
  :defer t
  :hook dired-mode)

(provide 'init-ui)
