;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Straight
(setq straight-use-package-by-default t)
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

(require 'init-keybindings)
(require 'init-ui)
(require 'init-utils)
(require 'init-backups)
(require 'init-settings)
(require 'init-editor)
(require 'init-project)
(require 'init-lsp)
(require 'init-eshell)
(require 'init-python)
(require 'init-javascript)
(require 'init-git)
(require 'init-spacebase)
(require 'init-org)
(require 'init-email)
(require 'init-lisp)
(require 'init-html)
(require 'init-tramp)
(require 'init-docker)
