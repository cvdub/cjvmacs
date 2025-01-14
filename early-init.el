;; Increase garbage control threshold during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq large-file-warning-threshold (* 1024 1024 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(add-hook 'after-init-hook (lambda ()
                             (message "Resetting GC threshold.")
                             (setq gc-cons-threshold (* 16 1024 1024))))

;; Set native compilation eln-cache directory
(defvar user-emacs-local-directory (expand-file-name ".local/" user-emacs-directory)
  "Directory for emacs cache files.")

(defvar user-emacs-cache-directory (expand-file-name "cache/" user-emacs-local-directory)
  "Directory for emacs cache files.")

(startup-redirect-eln-cache (expand-file-name "eln-cache" user-emacs-cache-directory))
