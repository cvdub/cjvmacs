;; Increase garbage control threshold during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq large-file-warning-threshold (* 1024 1024 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(add-hook 'after-init-hook (lambda ()
                             (message "Resetting GC threshold.")
                             (setq gc-cons-threshold (* 16 1024 1024))))

;; Store native comp files in var directory
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Disable double buffering (makes cursor movement feel snappier on MacOS)
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Add homebrew bin to path early so Emacs can find gcc for native comp
(setenv "PATH" "/opt/homebrew/bin/")

;; Load the the newest version of files
(setq load-prefer-newer t)

;; Enable transparent title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(setq use-package-compute-statistics nil)
