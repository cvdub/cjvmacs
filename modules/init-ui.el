;;;; Basic settings
(setq inhibit-startup-screen t)
(blink-cursor-mode -1)

(when window-system
  (set-scroll-bar-mode nil)
  (tool-bar-mode -1))

(setq use-system-tooltips nil
      use-file-dialog nil)

(winner-mode 1)

;;;; Mode line
(column-number-mode)

;;;; Font
(when window-system
  (set-face-attribute 'default nil :family "Fira Code" :height 140 :weight 'medium))


(custom-set-faces
 '(fixed-pitch ((t (:family "Fira Code" :height 140))))
 '(variable-pitch ((t (:family "iA Writer Quattro S" :height 140)))))

;;;; Theme
(use-package gruvbox-theme
  :defer t)

(custom-set-faces
 '(font-lock-constant-face ((t (:weight medium))))
 '(font-lock-keyword-face ((t (:weight medium)))))

(with-eval-after-load 'org
  (custom-set-faces
   '(org-document-title ((t (:height 1.3))))
   '(org-level-1 ((t (:weight extra-bold :height 1.25))))
   '(org-level-2 ((t (:weight bold :height 1.1))))
   '(org-tag ((t (:height .8))))
   '(org-document-info-keyword ((t (:height .9))))
   '(org-meta-line ((t (:height .9))))
   '(org-special-keyword ((t (:height .9))))
   '(org-date ((t (:inherit font-lock-comment-face :height .9))))
   '(org-drawer ((t (:height .9))))
   '(org-block-begin-line ((t (:height .9))))
   '(org-block-end-line ((t (:height .9))))
   '(org-quote ((t (:inherit variable-pitch :slant normal :family "iA Writer Quattro S")))))
  (custom-declare-face '+org-todo-todo '((t (:inherit (org-todo fixed-pitch) :weight bold))) "")
  (custom-declare-face '+org-todo-done '((t (:inherit (org-done fixed-pitch) :weight bold))) "")
  (custom-declare-face '+org-todo-onhold '((t (:inherit (font-lock-constant-face org-todo fixed-pitch) :weight bold))) "")
  (custom-declare-face '+org-todo-someday '((t (:inherit (font-lock-comment-face org-todo fixed-pitch) :weight bold))) ""))

(defvar cjv/light-theme 'gruvbox-light-medium)
(defvar cjv/dark-theme 'gruvbox-dark-medium)

(defun cjv/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (interactive (list (completing-read "Mode: " '(light dark))))
  (let* ((appearance (if (stringp appearance)
                         (intern appearance)
                       appearance))
         (theme (pcase appearance
                  ('light cjv/light-theme)
                  ('dark cjv/dark-theme))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (enable-theme theme)))

(defun cjv/customize-gruvbox-light-medium (theme)
  (when (eq theme 'gruvbox-light-medium)
    (custom-theme-set-faces
     'gruvbox-light-medium
     '(highlight ((t (:background "#ebdbb2" :foreground "#282828")))))))

(add-to-list 'enable-theme-functions 'cjv/customize-gruvbox-light-medium)

;; Switch theme when system dark/light mode changes
(add-hook 'ns-system-appearance-change-functions #'cjv/apply-theme)

;;;; Which Key
(use-package which-key
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

;;;; Tab Bar
(use-package tab-bar
  :elpaca nil
  :defer t
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-close-last-tab-choice tab-bar-mode-disable)
  (tab-bar-show nil))

;;;; Mode line
(use-package doom-modeline
  :ensure t
  :hook (cjv/after-init . doom-modeline-mode)
  :config
  (add-to-list 'doom-modeline-continuous-word-count-modes 'org-journal-mode)
  :custom
  (doom-modeline-enable-word-count t)
  (doom-modeline-battery nil)
  (doom-modeline-hud t))

;;;; Mixed pitch
(use-package mixed-pitch
  :defer t
  :hook (Info-mode . mixed-pitch-mode)
  :config
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-checkbox-statistics-todo))

;;;; Diff HL
(use-package diff-hl
  :defer t
  :hook (dired-mode . diff-hl-dired-mode-unless-remote)
  :init
  (global-diff-hl-mode)
  :custom
  (diff-hl-disable-on-remote t))

(provide 'init-ui)
