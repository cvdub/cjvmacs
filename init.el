;; Utils
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI apps are
not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
					  ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(defun hide-minor-mode (minor-mode)
  "Removes MINOR-MODE from minor-mode-alist."
  (setq minor-mode-alist (assq-delete-all minor-mode minor-mode-alist)))

;; Packages
(use-package package
  :config (package-initialize)
  :custom
  (package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
		      ("nongnu" . "https://elpa.nongnu.org/nongnu/")
		      ("melpa" . "https://melpa.org/packages/")))
  (package-user-dir (expand-file-name "packages/" user-emacs-local-directory))
  (package-native-compile t))

;; Emacs
(use-package cus-edit
  :config
  (load custom-file t)
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory)))

(use-package emacs
  :custom
  ;; Autosave
  (auto-save-no-message t)
  (remote-file-name-inhibit-auto-save t)
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (auto-save-list-file-prefix (expand-file-name "auto-save-list/saves-" user-emacs-cache-directory))
  ;; Backups
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (create-lockfiles nil))

(use-package bookmark
  :custom
  (bookmark-file (expand-file-name "bookmarks" user-emacs-local-directory)))

(use-package project
  :defer t
  :custom
  (project-list-file (expand-file-name "projects" user-emacs-local-directory)))

(use-package transient
  :defer t
  :custom
  (transient-history-file (expand-file-name "transient/history.el" user-emacs-local-directory))
  (transient-levels-file (expand-file-name "transient/levels.el" user-emacs-local-directory))
  (transient-values-file (expand-file-name "transient/values.el" user-emacs-local-directory)))

;; UI
(use-package emacs
  :custom
  (tool-bar-mode nil)
  (ring-bell-function 'silent)
  (use-short-answers t))

(use-package faces
  :config
  (message "loaded faces!")
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "Fira Code" :height 140)
    (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))
    (set-face-attribute 'variable-pitch nil :family "iA Writer Quattro V" :height 1.0)
    (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji" :size 11))))

(use-package modus-themes
  :ensure t
  :after faces
  :config
  (message "Loaded modus!")
  (modus-themes-load-theme 'modus-operandi)
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t))

(use-package custom
  :custom
  (custom-enabled-themes '(modus-operandi)))

(use-package frame
  :custom
  (blink-cursor-mode nil))

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil))

(use-package hl-line
  :custom
  (global-hl-line-mode t))

;; Completion
(use-package which-key
  :custom
  (which-key-mode t)
  (which-key-compute-remaps t)
  (which-key-idle-delay 0.75)
  (which-key-lighter nil))

(use-package icomplete
  :custom
  (fido-vertical-mode t))

(use-package scroll-bar
  :custom
  (scroll-bar-mode nil))

(use-package minibuffer
  :custom
  (completion-styles '(basic flex))
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-max-height 20)
  (completions-sort 'historical))

(use-package simple
  :custom
  (completion-auto-select t)
  (completion-show-help nil)
  (indent-tabs-mode nil)
  (column-number-mode t))

(use-package completion-preview
  :bind (:map completion-preview-active-mode-map
	      ("M-n" . #'completion-preview-next-candidate)
	      ("M-p". #'completion-preview-prev-candidate))
  :config
  (hide-minor-mode 'completion-preview-mode)
  :custom
  (completion-preview-message-format "%i/%n possible completions")
  (global-completion-preview-mode t))

(use-package savehist
  :custom
  (savehist-mode t)
  (savehist-autosave-interval 60)
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-file (expand-file-name "history" user-emacs-cache-directory)))

(use-package recentf
  :custom
  (recentf-mode t)
  (recentf-save-file (expand-file-name "recentf" user-emacs-cache-directory))
  (recentf-max-saved-items 500)
  (recentf-auto-cleanup 'never))

;; Editor
(use-package delsel
  :custom
  (delete-selection-mode t))

(use-package files
  :custom
  (require-final-newline t))

(use-package autorevert
  :custom
  (global-auto-revert-mode t))

(use-package ffap
  :config (ffap-bindings))

(use-package editorconfig
  :custom
  (editorconfig-mode t))

(use-package elec-pair
  :custom
  (electric-pair-mode t))

;; Tools
(use-package eshell
  :defer t
  :custom
  (eshell-directory-name (expand-file-name "eshell/" user-emacs-cache-directory)))

(use-package tramp
  :defer t
  :custom
  (tramp-persistency-file-name (expand-file-name "tramp" user-emacs-local-directory)))
