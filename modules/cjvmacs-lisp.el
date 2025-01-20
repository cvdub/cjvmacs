;;; cjvmacs-lisp.el --- Lisp config for CJVmacs      -*- lexical-binding: t; -*-

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

;; Lisp config for CJVmacs

;;; Code:

(use-package paredit
  :ensure t
  :defer t
  :diminish paredit-mode
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)))

(use-package inf-lisp
  :defer t
  :custom (inferior-lisp-program "sbcl"))

(use-package sly
  :ensure t
  :defer t
  :bind (:map sly-mode-map
              ("C-c o r" . cjv/lisp-open-repl))
  :config
  (defun cjv/lisp-open-repl ()
    "Opens sly REPL in a bottom side window."
    (interactive)
    (sly-mrepl)
    (cjv/with-bottom-window
     (if (sly-connected-p)
         (switch-to-buffer-other-window (sly-mrepl #'display-buffer))
       (sly))))
  :custom
  (sly-auto-start 'always)
  (sly-command-switch-to-existing-lisp 'always))

(use-package sly-asdf
  :ensure t
  :after sly
  :init (add-to-list 'sly-contribs 'sly-asdf 'append)
  :bind (:map sly-mode-map
              ("C-c c l" . #'sly-asdf-load-system)))

;; (use-package elisp-mode
;; :hook (emacs-lisp-mode-hook . flymake-mode)
;; :init
;; (setq elisp-flymake-byte-compile-load-path load-path)
;; :custom
;; (elisp-flymake-byte-compile-load-path load-path)
;; )

(provide 'cjvmacs-lisp)

;;; cjvmacs-lisp.el ends here
