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
  (set-face-attribute 'default nil :family "Fira Code" :height 140)
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji" :size 11)))

(custom-set-faces
 '(fixed-pitch ((t (:family "Fira Code" :height 140))))
 '(variable-pitch ((t (:family "iA Writer Quattro V" :height 140 :weight medium)))))

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
   '(org-block-begin-line ((t (:inherit org-special-keyword
                                        :height .8
                                        :background unspecified
                                        :underline (:color foreground-color
                                                           :style line
                                                           :position t)))))
   '(org-block-end-line ((t (:inherit org-special-keyword
                                      :height .8
                                      :background unspecified
                                      :underline (:color foreground-color
                                                         :style line
                                                         :position 16)))))
   '(org-quote ((t (:inherit variable-pitch :slant normal :family "iA Writer Quattro V" :weight medium)))))
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

;; (defun cjv/customize-gruvbox-dark-medium (theme)
;;   (when (eq theme 'gruvbox-dark-medium)
;;     (custom-theme-set-faces
;;      'gruvbox-dark-medium
;;      '(internal-border ((t (:background "#282828" :foreground "#262626")))))))

(add-to-list 'enable-theme-functions 'cjv/customize-gruvbox-light-medium)
;; (add-to-list 'enable-theme-functions 'cjv/customize-gruvbox-dark-medium)

(defun cjv/mac-set-theme-based-on-system ()
  "Sets Emacs theme based on Mac system dark/light."
  (let ((appearance (plist-get (mac-application-state) :appearance)))
    (message appearance)
    (if (string= appearance "NSAppearanceNameDarkAqua")
        (cjv/apply-theme 'dark)
      (cjv/apply-theme 'light))))

;; Enable theme
(add-hook 'elpaca-after-init-hook #'cjv/mac-set-theme-based-on-system)
;; (cjv/mac-set-theme-based-on-system)

;; Switch theme when system dark/light mode changes
(add-hook 'mac-effective-appearance-change-hook #'cjv/mac-set-theme-based-on-system)

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
  :hook visual-line-mode
  :bind (:map cjv/toggle-map
              ("c" . #'cjv/visual-line-mode-toggle-center))
  :config
  (defun cjv/visual-line-mode-toggle-center ()
    "Toggles centering text in visual line mode."
    (interactive)
    (setq-local visual-fill-column-center-text (not visual-fill-column-center-text))
    (visual-fill-column-adjust)))

(use-package adaptive-wrap)

;;;; Write room
(use-package writeroom-mode
  :defer t
  :hook (writeroom-mode . cjv/writeroom-increase-text-scaling)
  :bind (:map cjv/toggle-map
              ("w" . #'writeroom-mode))
  :config
  (defun cjv/writeroom-increase-text-scaling ()
    (if writeroom-mode
        (text-scale-adjust 1)
      (text-scale-adjust 0))
    (visual-fill-column-adjust))
  :custom
  (writeroom-mode-line t)
  (writeroom-fullscreen-effect 'maximized))

;;;; Icons
(use-package all-the-icons)

;;;; Tab Bar
(use-package tab-bar
  :ensure nil
  :init (tab-bar-mode 1)
  :bind (:map tab-bar-map
              ("C-<tab>" . #'tab-next))
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-close-last-tab-choice tab-bar-mode-disable)
  (tab-bar-show nil))

;;;; Mode line
(use-package doom-modeline
  :defer t
  :hook (elpaca-after-init . doom-modeline-mode)
  :config
  (add-to-list 'doom-modeline-continuous-word-count-modes 'org-journal-mode)

  ;; Add tab name to vcs modeline
  (doom-modeline-def-modeline 'vcs
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(compilation misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process time))

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

;;;; hl-line
(add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))

;;;; Diff HL
(use-package diff-hl
  :defer 5
  :hook (dired-mode . diff-hl-dired-mode-unless-remote)
  :config
  (global-diff-hl-mode)
  :custom
  (diff-hl-disable-on-remote))

;;;; Ligatures
;; (mac-auto-operator-composition-mode)

;;;; Goggles
(use-package goggles
  :defer t
  :hook ((prog-mode text-mode) . goggles-mode))

;;;; Solaire mode
;; (use-package solaire-mode
;;   :init (solaire-global-mode +1))

;;;; Dashboard
(use-package dashboard
  :ensure (dashboard :fetcher github :repo "emacs-dashboard/emacs-dashboard"
                     :remotes (("fork" :repo "cvdub/emacs-dashboard" :protocol ssh)))
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook)
  (setq dashboard-force-refresh t)
  :custom
  (dashboard-startup-banner (expand-file-name "emacs-logo-gruvbox-dark.svg" user-emacs-directory))
  (dashboard-banner-logo-title nil)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-items nil)
  (dashboard-footer-messages '("The one true editor, Emacs!"
                               "Free as free speech, free as free Beer"
                               "Happy coding!"
                               "Welcome to the church of Emacs"
                               "While any text editor can save your files, only Emacs can save your soul"))
  (dashboard-hide-cursor t)
  :custom-face
  (dashboard-footer-face ((t (:foreground "#83a598")))))

;;;; Rainbow mode
;; (use-package rainbow-mode
;;   :hook (prog-mode . rainbow-mode)
;;   )

;;;; Indent bars
(use-package indent-bars
  :ensure (indent-bars :fetcher github :repo "jdtsmith/indent-bars")
  :bind (:map cjv/toggle-map
              ("i" . #'indent-bars-mode))
  :config
  (require 'indent-bars-ts)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	                                     if_statement with_statement while_statement))))

;;;; Info colors
(use-package info-colors
  :ensure (info-colors :fetcher github :repo "ubolonton/info-colors")
  :config (add-hook 'Info-selection-hook 'info-colors-fontify-node))

(provide 'init-ui)
