;; Increase garbage control threshold during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq large-file-warning-threshold (* 1024 1024 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(add-hook 'after-init-hook (lambda ()
                             (message "Resetting GC threshold.")
                             (setq gc-cons-threshold (* 16 1024 1024))))

;; Set native compilation eln-cache directory
(defvar user-emacs-local-directory (expand-file-name ".local/" user-emacs-directory)
  "Directory for Emacs cache files.")

(defvar user-emacs-cache-directory (expand-file-name "cache/" user-emacs-local-directory)
  "Directory for Emacs cache files.")

;; Set these variables early to ensure emacs dir isn't cluttered
(startup-redirect-eln-cache (expand-file-name "eln-cache" user-emacs-cache-directory))
(setq elisp-flymake-byte-compile-load-path load-path
      savehist-file (expand-file-name "history" user-emacs-cache-directory))

;; Disable double buffering (makes cursor movement feel snappier on MacOS)
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Add homebrew bin to path early so Emacs can find gcc for native comp
(setenv "PATH" "/opt/homebrew/bin/")

;; Configure package.el
(setq package-user-dir (expand-file-name "packages/" user-emacs-local-directory)
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
		         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
		         ("melpa" . "https://melpa.org/packages/"))
      package-native-compile t)

(setq load-prefer-newer t)

;; (setq use-package-compute-statistics t)
