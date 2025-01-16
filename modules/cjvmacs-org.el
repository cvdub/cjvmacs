;;; cjvmacs-org.el --- Org mode config for CJVmacs   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Christian Vanderwall

;; Author: Christian Vanderwall <christian@cvdub.net>
;; Keywords: 

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

;; Org mode config for CJVmacs

;;; Code:

(use-package org
  :ensure t
  :defer t
  :commands (cjv/org-open-work-todo-file
             cjv/org-open-personal-todo-file)
  :hook (org-mode . variable-pitch-mode)
  :bind (("<f10>" . #'org-agenda)
         ("<C-f10>" . #'cjv/org-open-work-todo-file)
         ("<S-f10>" . #'cjv/org-open-personal-todo-file)
         :map org-mode-map
         ("M-p" . #'org-metaup)
         ("M-n" . #'org-metadown))
  :config
  (defun cjv/org-open-work-todo-file ()
    "Open work todo file."
    (interactive)
    (find-file "~/Documents/org/work.org"))

  (defun cjv/org-open-personal-todo-file ()
    "Open personal todo file."
    (interactive)
    (find-file "~/Documents/org/home.org"))

  :custom
  (org-directory "~/Documents/org/")
  (org-agenda-files (list org-directory))
  (org-archive-location (expand-file-name "archive/archive.org_archive::* From %s" org-directory))
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-k t)
  (org-special-ctrl-e t)
  (org-return-follows-link t)
  (org-use-speed-commands t)
  (org-startup-indented t)
  (org-startup-folded 'show2levels)

  (org-use-fast-todo-selection 'expert)
  (org-todo-keywords '((sequence
                        "TODO(t)"
                        "DELE(l)"
                        "WAIT(w@)"
                        "HOLD(h@)"
                        "SOME(s)"
                        "|"
                        "DONE(d)"
                        "CANC(c)"))))

(provide 'cjvmacs-org)

;;; cjvmacs-org.el ends here
