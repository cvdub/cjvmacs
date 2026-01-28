;;; cjvmacs-html.el --- HTML config for CJVmacs      -*- lexical-binding: t; -*-

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

;; HTML config for CJVmacs

;;; Code:

(use-package web-mode
  :defer t
  :mode ("\\.html?\\'"
         "\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'")
  :init
  ;; Disable pairing {/}
  (add-hook
   'web-mode-hook
   '(lambda ()
      (setq-local electric-pair-inhibit-predicate
                  (lambda (c)
                    (if (char-equal c ?{) t (electric-pair-default-inhibit c))))))
  :custom
  (web-mode-extra-control-blocks '(("django" . ("partialdef" "endpartialdef" "partial")))))


(provide 'cjvmacs-html)

;;; cjvmacs-html.el ends here
