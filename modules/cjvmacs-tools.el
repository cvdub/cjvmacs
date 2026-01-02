;;; cjvmacs-tools.el --- Tools config for CJVmacs    -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Christian Vanderwall

;; Author: Christian Vanderwall <christian@cvdub.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tools config for CJVmacs

;;; Code:

(use-package eshell
  :defer t
  :commands cjv/open-eshell
  :bind (:map cjv/open-map
              ("e" . #'cjv/open-eshell))
  :hook ((eshell-mode . (lambda ()
                          (remove-hook 'eshell-output-filter-functions 'eshell-postoutput-scroll-to-bottom)))
         (eshell-mode . cjv/eshell-refresh-dir-local-variables)
         (eshell-directory-change . cjv/eshell-refresh-dir-local-variables))
  :config
  (defun cjv/open-eshell (&optional prefix)
    "Opens eshell in a bottom side window."
    (interactive "P")
    (if prefix
        (eshell)
      (cjv/with-bottom-window (eshell))))

  (add-to-list 'eshell-modules-list 'eshell-tramp)

  (defun cjv/eshell-prompt ()
    (let ((cwd (abbreviate-file-name (eshell/pwd)))
          (sym (if (= (user-uid) 0) "# " "$ ")))
      (concat
       ;; line 1: cwd
       cwd
       "\n"
       ;; line 2: prompt symbol
       sym)))

  (setq eshell-prompt-function #'cjv/eshell-prompt
        eshell-prompt-regexp "^[^#$\n]*\n[#$] ")

  (defun cjv/eshell-refresh-dir-local-variables ()
    "Clear and reload dir-local variables."
    (dolist (entry dir-local-variables-alist)
      (kill-local-variable (car entry)))
    (setq file-local-variables-alist nil
          dir-local-variables-alist nil)
    (hack-dir-local-variables-non-file-buffer))

  :custom
  (eshell-visual-commands '("vi" "vim" "nvim" "screen" "tmux" "top" "htop" "less" "more" "lynx" "links" "ncftp" "ncmpcpp"
                            "mutt" "pine" "tin" "trn" "elm"))
  (eshell-visual-subcommands '(("docker" "build")))
  (eshell-directory-name (expand-file-name "eshell/" user-emacs-cache-directory))
  (eshell-banner-message "")
  (eshell-banner-message "")
  (eshell-history-size 100000)
  (eshell-hist-ignoredumps t)
  (eshell-destroy-buffer-when-process-dies t))

(use-package eat
  :ensure t
  :defer t
  :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

(use-package tramp
  :defer t
  :custom
  (tramp-persistency-file-name (expand-file-name "tramp" user-emacs-local-directory)))

(use-package magit
  :ensure t
  :defer t
  :bind (:map magit-section-mode-map
              ("C-<tab>" . nil))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-database-file (expand-file-name "forge-database.sqlite" user-emacs-local-directory)))

(use-package ansible
  :ensure t
  :defer t
  :custom
  (ansible-vault-password-file "/Users/cjv/.config/ansible/vault-password.sh"))

(use-package autothemer
  :ensure t
  :defer t)

(use-package yaml-ts-mode
  :defer t
  :mode ("\\.ya?ml\\'"))

(use-package pdf-tools
  :ensure t
  :defer t
  :init
  (pdf-loader-install))


(use-package systemd
  :ensure t
  :defer t)

(use-package sudo-edit
  :ensure t
  :defer t)

(use-package jinja2-mode
  :ensure t
  :defer t
  :mode ("\\.jinja\\'"))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package applescript-mode
  :ensure t)

(use-package terraform-mode
  :ensure t
  :defer t)

(use-package ledger-mode
  :ensure t
  :defer t)

(use-package csv-mode
  :ensure t
  :defer t
  :hook (csv-mode . (lambda () (visual-line-fill-column-mode -1))))

(use-package htmlize
  :ensure t)

(provide 'cjvmacs-tools)

;;; cjvmacs-tools.el ends here
