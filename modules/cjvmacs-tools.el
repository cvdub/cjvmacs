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
  :hook (eshell-mode-hook . (lambda ()
                              (remove-hook 'eshell-output-filter-functions 'eshell-postoutput-scroll-to-bottom)))
  :config
  (defun cjv/open-eshell ()
    "Opens eshell in a bottom side window."
    (interactive)
    (cjv/with-bottom-window (eshell)))

  (add-to-list 'eshell-modules-list 'eshell-tramp)
  :custom
  (eshell-directory-name (expand-file-name "eshell/" user-emacs-cache-directory))
  (eshell-banner-message "")
  (eshell-banner-message "")
  (eshell-history-size 100000)
  (eshell-hist-ignoredumps t)
  (eshell-destroy-buffer-when-process-dies t))

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

;; (use-package forge
;;   :ensure t
;;   :after magit
;;   :custom
;;   (forge-owned-accounts '(("cvdub" remote-name "fork"))))

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



(provide 'cjvmacs-tools)

;;; cjvmacs-tools.el ends here
