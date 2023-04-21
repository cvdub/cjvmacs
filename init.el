;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Garbage collection
(setq gc-cons-threshold 50000000) ; 50MB
(setq large-file-warning-threshold 100000000)

;; Straight
(setq straight-use-package-by-default t
      straight-vc-git-default-protocol 'ssh)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Modules
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

(require 'init-ui)
(require 'init-utils)
(require 'init-backups)
(require 'init-settings)
(require 'init-editor)
(require 'init-completion)
(require 'init-eshell)
(require 'init-python)
(require 'init-git)
(require 'init-spacebase)

