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
  :diminish org-indent-mode
  :commands (cjv/org-open-agenda
             cjv/org-open-work-todo-file
             cjv/org-open-personal-todo-file
             org-with-point-at
             cjv/org-punch-in
             cjv/org-punch-out)
  :hook (org-mode . variable-pitch-mode)
  :bind (("<f10>" . #'cjv/org-open-agenda)
         ("<C-f10>" . #'cjv/org-open-work-todo-file)
         ("<S-f10>" . #'cjv/org-open-personal-todo-file)
         :map cjv/notes-map
         ("n" . #'org-capture)
         ("l" . #'org-store-link)
         ("o" . #'org-clock-goto)
         ("p" . #'cjv/org-punch-in)
         ("P" . #'cjv/org-punch-out)
         ("a" . #'cjv/org-capture-apple-reminders)
         :map org-mode-map
         ("M-p" . #'org-metaup)
         ("M-n" . #'org-metadown)
         :map org-agenda-mode-map
         ("C-t" . #'org-agenda-todo-yesterday))
  :init
  ;; Create TODO faces
  (custom-declare-face 'org-todo-someday '((t (:inherit (font-lock-comment-face org-todo)))) "Face for SOME keywords.")
  (custom-declare-face 'org-todo-done '((t (:inherit org-done))) "Face for DONE keywords.")
  (custom-declare-face 'org-todo-onhold '((t (:inherit (font-lock-constant-face org-todo) :weight bold))) "Face for HOLD keywords.")

  :config
  (add-to-list 'completion-preview-commands #'org-self-insert-command)

  (defvar cjv/org-inbox-file (expand-file-name "inbox.org" org-directory)
    "My org-mode inbox file.")

  ;; Modules
  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)
  (require 'ox-md)                      ; Markdown

  ;; Agenda functions
  (defun cjv/org-agenda-element-padding ()
    (cl-flet ((todayp (time-str)
                (and time-str
                     (time-equal-p (org-time-string-to-time time-str)
                                   (org-time-string-to-time (format-time-string "%Y-%m-%d"))))))
      (let ((timestamp (org-entry-get (point) "TIMESTAMP"))
            (scheduled (org-entry-get (point) "SCHEDULED"))
            (deadline (org-entry-get (point) "DEADLINE"))
            (style (org-entry-get (point) "STYLE")))
        (cond (timestamp "---- ")
              ((string= style "habit") "")
              ((todayp scheduled) "Scheduled:  ")
              ((todayp deadline) "Deadline:   ")
              (t "")))))

  (defun cjv/org-agenda-priority-padding ()
    (let ((priority (org-entry-get (point) "PRIORITY")))
      (if (string= priority "B")
          "     "
        (format "[#%s] " priority))))

  (defun cjv/org-agenda-format-item (s)
    "Format string before display in org agenda."
    (replace-regexp-in-string "\\(\\[#[A-Z0-9]+\\] \\).*\\(\\[#[A-Z0-9]+\\] \\)" "" s nil nil 2))
  (advice-add 'org-agenda-format-item  :filter-return #'cjv/org-agenda-format-item)

  (defun cjv/org-subtask-p ()
    "Returns true if the heading at point is a subtask."
    (let ((subtask-p nil))
      (save-excursion
        (while (and (not subtask-p) (not (= (org-outline-level) 1)))
          (outline-up-heading 1 t)
          (if (org-get-todo-state)
              (setq subtask-p t))))
      subtask-p))

  (defun cjv/org-first-todo-p ()
    "Returns t if the heading at point is the subtree's first TODO."
    (let ((first-todo-p (string= (org-entry-get (point) "TODO") "TODO")))
      (save-excursion
        (while (and first-todo-p (save-excursion (org-get-previous-sibling)))
          (org-backward-heading-same-level 1 t)
          (setq first-todo-p (not (string= (org-entry-get (point) "TODO") "TODO")))))
      first-todo-p))

  (defun cjv/org-next-action-p ()
    "Returns t if the heading at point is a next action."
    (and (string= (org-entry-get (point) "TODO") "TODO")
         (not (cjv/org-subtask-p))
         (or (org-entry-get (point) "GTD_SECTION" t)
             (cjv/org-first-todo-p))))

  (defun cjv/org-agenda-skip-function ()
    (if (not (cjv/org-next-action-p))
        (save-excursion (outline-next-heading) (1- (point)))
      (org-agenda-skip-entry-if 'deadline 'scheduled)))

  ;; Show full subtree of matching items when narrowing to sparse trees.
  (push '(occur-tree . ancestors-full) org-show-context-detail)

;;;; Capture
  ;; Display capture buffers in bottom side window
  (add-to-list 'display-buffer-alist
               '("\\*Org Select\\*"
                 (display-buffer-in-side-window)
                 (inhibit-same-window . nil)
                 (side . bottom)))
  (add-to-list 'display-buffer-alist
               '("CAPTURE-inbox.org"
                 (display-buffer-in-side-window)
                 (inhibit-same-window . nil)
                 (window-height . 0.2)
                 (side . bottom)))

  ;; Don't delete other windows when opening capture template
  (defun cjv/org-capture-place-template-dont-delete-windows (oldfun &rest args)
    (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
      (apply oldfun args)))

  (with-eval-after-load "org-capture"
    (advice-add 'org-capture-place-template
                :around 'cjv/org-capture-place-template-dont-delete-windows))

  ;; Clock
  (defvar cjv/org-punched-in-task-id nil
    "The task ID of the default task. Nil if not punched in.")

  (defvar cjv/org-default-tasks
    '(("Work" . "7b2f58f0-955d-487b-bc91-93b788b59ed0")
      ("Home" . "066813ec-f434-4a4d-a4ae-ed473401675b")))

  (defun cjv/org-get-default-task-id ()
    "Get the task ID from the list of default tasks."
    (cdr (assoc (completing-read "Select task: " cjv/org-default-tasks)
                cjv/org-default-tasks)))

  (defun cjv/org-punch-in ()
    "Punch in and start clock on default task."
    (interactive)
    (setq cjv/org-punched-in-task-id (cjv/org-get-default-task-id))
    (org-with-point-at (org-id-find cjv/org-punched-in-task-id 'marker)
      (org-clock-in '(16))))

  (defun cjv/org-punch-out ()
    "Stop clock and punch out."
    (interactive)
    (setq cjv/org-punched-in-task-id nil)
    (org-clock-out))

  (defun cjv/org-maybe-clock-into-punched-in-task ()
    "Clocks back into the default task if punched in."
    (when (and cjv/org-punched-in-task-id
               (not org-clock-clocking-in)
               (marker-buffer org-clock-default-task)
               (not org-clock-resolving-clocks-due-to-idleness))
      (save-excursion
        (org-with-point-at (org-id-find cjv/org-punched-in-task-id 'marker)
          (org-clock-in)))))

  (add-hook 'org-clock-out-hook #'cjv/org-maybe-clock-into-punched-in-task 'append)

  (defun cjv/org-clock-heading-function ()
    "Returns the punched in task name or heading name for the mode line."
    (or (car (rassoc (org-id-get) cjv/org-default-tasks))
        (org-link-display-format
         (org-no-properties (org-get-heading t t t t)))))

  (setq org-clock-heading-function #'cjv/org-clock-heading-function)

  ;; Shortcuts
  (defun cjv/org-open-agenda ()
    "Open custom org agenda."
    (interactive)
    (org-agenda nil "b"))

  (defun cjv/org-open-work-todo-file ()
    "Open work todo file."
    (interactive)
    (find-file "~/Documents/org/work.org"))

  (defun cjv/org-open-personal-todo-file ()
    "Open personal todo file."
    (interactive)
    (find-file "~/Documents/org/home.org"))

  (defun cjv/org-capture-apple-reminders ()
    (interactive)
    (cjv/with-message
     "Done!"
     (start-process-shell-command "Org Capture Apple Reminders"
                                  "*org capture apple reminders*"
                                  "osascript /Users/cjv/code/scripts/capture-reminders.scpt")))

  :custom
  ;; Files
  (org-directory "~/Documents/org/")
  (org-agenda-files (list org-directory))
  (org-archive-location (expand-file-name "archive/archive.org_archive::* From %s" org-directory))

  ;; Behavior
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-k t)
  (org-special-ctrl-e t)
  (org-return-follows-link t)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-log-note-headings '((done . "CLOSING NOTE %t")
                           (state . "State %s from %S %t")
                           (note . "Note taken on %t")
                           (reschedule . "Rescheduled from %S on %t")
                           (delschedule . "Not scheduled, was %S on %t")
                           (redeadline . "New deadline from %S on %t")
                           (deldeadline . "Removed deadline, was %S on %t")
                           (refile . "Refiled on %t")
                           (clock-out . "")))
  (org-reverse-note-order t)

  ;; Enable speed commands, and activate t hem on any of the asterisks
  ;; at the beginning of the line.
  (org-use-speed-commands (lambda ()
                            (and (looking-at org-outline-regexp)
                                 (looking-back "^\**"))))
  (org-fast-tag-selection-single-key t)
  (org-outline-path-complete-in-steps nil)

  ;; Clock
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-clocked-in-display 'frame-title)
  (org-clock-mode-line-total 'today)

  ;; Refile
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))

  ;; UI
  (org-startup-indented t)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))
  (org-startup-folded 'show2levels)
  (org-startup-align-all-tables t)
  (org-startup-shrink-all-tables t)
  (org-startup-truncated nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-tags-column 0)
  (org-hide-emphasis-markers t)


  ;; Export
  (org-export-with-toc nil)
  (org-export-with-section-numbers nil)
  (org-export-with-sub-superscripts '{})

  ;; Todo
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)
  (org-todo-keywords '((sequence
                        "TODO(t)"
                        "DELE(l)"
                        "WAIT(w@)"
                        "HOLD(h@)"
                        "SOME(s)"
                        "|"
                        "DONE(d)"
                        "CANC(c)")))
  (org-todo-keyword-faces
   '(("TODO" . org-todo)
     ("DELE" . org-todo-onhold)
     ("SOME" . org-todo-someday)
     ("HOLD" . org-todo-onhold)
     ("WAIT" . org-todo-onhold)
     ("DONE" . org-todo-done)
     ("CANC" . org-todo-done)))

  ;; Capture
  (org-capture-templates
   '(("t" "Todo" entry (file+headline cjv/org-inbox-file "Inbox")
      "* TODO %i%?")
     ("T" "Todo with link" entry (file+headline cjv/org-inbox-file "Inbox")
      "* TODO %i%?\n- %a")))

  ;; Agenda
  (org-agenda-tags-column -120)
  (org-agenda-window-setup 'current-window)
  (org-agenda-sorting-strategy '((agenda habit-down time-up scheduled-up priority-down)
                                 (todo priority-down category-keep)
                                 (tags priority-down category-keep)
                                 (search category-keep)))
  (org-agenda-custom-commands
   '(("h" "Agenda and home next actions"
      ((agenda ""
               ((org-agenda-span 1)
                (org-agenda-start-on-weekday nil)
                (org-agenda-dim-blocked-tasks 'nil)
                (org-habit-show-habits nil)))
       (todo "TODO"
             ((org-agenda-overriding-header "\nHome")
              (org-agenda-files '("~/Documents/org/home.org"))
              (org-agenda-dim-blocked-tasks 'invisible)
              (org-agenda-skip-function 'cjv/org-agenda-skip-function)))
       (agenda ""
               ((org-agenda-overriding-header "\nHabits")
                (org-agenda-files '("~/Documents/org/habits.org"))
                (org-agenda-span 1)
                (org-agenda-start-on-weekday nil)))))
     ("w" "Agenda and work next actions"
      ((agenda ""
               ((org-agenda-span 1)
                (org-agenda-start-on-weekday nil)
                (org-agenda-dim-blocked-tasks 'nil)
                (org-habit-show-habits nil)))
       (todo "TODO"
             ((org-agenda-overriding-header "\nWork")
              (org-agenda-files '("~/Documents/org/work.org"))
              (org-agenda-dim-blocked-tasks 'invisible)
              (org-agenda-skip-function 'cjv/org-agenda-skip-function)))))
     ("b" "Agenda and next actions"
      ((agenda ""
               ((org-agenda-overriding-header "\nWork Agenda")
                (org-agenda-span 1)
                (org-agenda-start-on-weekday nil)
                (org-agenda-dim-blocked-tasks 'nil)
                (org-habit-show-habits nil)
                (org-agenda-files '("~/Documents/org/work.org"))))
       (agenda ""
               ((org-agenda-overriding-header "\nHome Agenda")
                (org-agenda-span 1)
                (org-agenda-start-on-weekday nil)
                (org-agenda-dim-blocked-tasks 'nil)
                (org-habit-show-habits nil)
                (org-agenda-files '("~/Documents/org/home.org"))))
       (todo "TODO"
             ((org-agenda-overriding-header "\nWork Tasks")
              (org-agenda-files '("~/Documents/org/work.org"))
              (org-agenda-dim-blocked-tasks 'nil)
              (org-agenda-skip-function 'cjv/org-agenda-skip-function)))
       (todo "TODO"
             ((org-agenda-overriding-header "\nHome Tasks")
              (org-agenda-files '("~/Documents/org/home.org"))
              (org-agenda-dim-blocked-tasks 'nil)
              (org-agenda-skip-function 'cjv/org-agenda-skip-function)))
       (agenda ""
               ((org-agenda-overriding-header "\nHabits")
                (org-agenda-files '("~/Documents/org/habits.org"))
                (org-agenda-span 1)
                (org-agenda-start-on-weekday nil)))))
     ("e" "Errands" ((tags-todo "#ERRAND/TODO")))
     ("y" "Someday" ((todo "SOME")))))
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-deadline-warning-days 14)
  (org-agenda-start-day nil)
  (org-agenda-block-separator nil)
  (org-agenda-use-time-grid t)
  (org-agenda-time-grid '((daily today require-timed remove-match)
                          (800 1000 1200 1400 1600 1800 2000)
                          "......" "----------------"))
  (org-agenda-scheduled-leaders '("" "Sched.%2dx: "))
  (org-agenda-deadline-leaders '("" "In %3d d.: " "%2d d. ago: "))
  (org-agenda-prefix-format '((agenda . "%(cjv/org-agenda-priority-padding)%-12:c%?-12t% s%(cjv/org-agenda-element-padding)")
                              (todo . "%(cjv/org-agenda-priority-padding)%-12:c")
                              (tags . "%-12:c")
                              (search . "%-12:c")))
  (org-habit-graph-column 45)
  (org-habit-preceding-days 7)
  (org-habit-following-days 1)

  ;; Babel
  (org-babel-load-languages '((emacs-lisp . t)
                              (lisp . t)
                              (python . t)
                              (shell . t)))
  (org-babel-python-command-nonsession "python3"))

(use-package org-journal
  :ensure t
  :defer t
  :bind (:map cjv/notes-map
              ("j" . #'org-journal-new-entry)
              :map org-journal-mode-map
              ("C-c l n" . #'org-journal-next-entry)
              ("C-c l p" . #'org-journal-previous-entry))
  :init
  (setq org-journal-prefix-key nil)
  :hook (org-journal-mode . writeroom-mode)
  :custom
  (org-journal-dir (expand-file-name "journal/" org-directory)))

(defun cjv/org-capture-place-template-dont-delete-windows (oldfun &rest args)
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
    (apply oldfun args)))

(with-eval-after-load "org-capture"
  (advice-add 'org-capture-place-template :around 'cjv/org-capture-place-template-dont-delete-windows))

(use-package org-roam
  :ensure t
  :defer t
  :after org
  :bind (:map cjv/notes-map
              :prefix-map cjv/org-roam-map
              :prefix "r"
              ("f" . #'org-roam-node-find))
  :config
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-directory (expand-file-name "roam/" org-directory))
  (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  (org-roam-completion-everywhere t))

(provide 'cjvmacs-org)

;;; cjvmacs-org.el ends here
