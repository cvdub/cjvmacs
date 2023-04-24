(use-package org
  :straight (:type built-in)
  :defer t
  :commands (cjv/org-open-agenda
             cjv/org-open-work-todo-file
             cjv/org-open-personal-todo-file)
  :bind (("<f10>" . #'cjv/org-open-agenda)
         ("<C-f10>" . #'cjv/org-open-work-todo-file)
         ("<S-f10>" . #'cjv/org-open-personal-todo-file)
         ("M-p" . #'org-metaup)
         ("M-n" . #'org-metadown))
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode))

  :config
  ;; Modules
  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)

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

  ;; Shortcuts
  (defun cjv/org-open-agenda ()
    (interactive)
    "Open custom org agenda."
    (org-agenda nil "b"))

  (defun cjv/org-open-work-todo-file ()
    (interactive)
    "Open work todo file."
    (find-file "~/Documents/org/work.org"))

  (defun cjv/org-open-personal-todo-file ()
    (interactive)
    "Open personal todo file."
    (find-file "~/Documents/org/home.org"))

  :custom
  ;; Files
  (org-directory "~/Documents/org/")
  (org-agenda-files (list org-directory))
  (org-archive-location (expand-file-name "archive/archive.org_archive::* From %s" org-directory))

  ;; Behavior
  (org-special-ctrl-k t)
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

  ;; Enable speed commands, and activate t hem on any of the asterisks
  ;; at the beginning of the line.
  (org-use-speed-commands (lambda ()
                            (and (looking-at org-outline-regexp)
                                 (looking-back "^\**"))))
  (org-fast-tag-selection-single-key t)

  ;; UI
  (org-startup-indented t)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))
  (org-startup-folded 'show2levels)
  (org-startup-align-all-tables t)
  (org-startup-shrink-all-tables t)
  (org-startup-truncated nil)
  (org-fontify-quote-and-verse-blocks t)

  ;; Export
  (org-export-with-toc nil)
  (org-export-with-section-numbers nil)
  (org-export-with-sub-superscripts '{})

  ;; Todo
  (org-use-fast-todo-selection 'expert)
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
   '(("TODO" . +org-todo-todo)
     ("DELE" . +org-todo-onhold)
     ("SOME" . +org-todo-someday)
     ("HOLD" . +org-todo-onhold)
     ("WAIT" . +org-todo-onhold)
     ("DONE" . +org-todo-done)
     ("CANC" . +org-todo-done)))

  ;; Agenda
  (org-agenda-window-setup 'current-window)
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
               ((org-agenda-span 1)
                (org-agenda-start-on-weekday nil)
                (org-agenda-dim-blocked-tasks 'nil)
                (org-habit-show-habits nil)))
       (todo "TODO"
             ((org-agenda-overriding-header "\nWork")
              (org-agenda-files '("~/Documents/org/work.org"))
              (org-agenda-dim-blocked-tasks 'nil)
              (org-agenda-skip-function 'cjv/org-agenda-skip-function)))
       (todo "TODO"
             ((org-agenda-overriding-header "\nHome")
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
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s%(cjv/org-agenda-element-padding)")
                              (todo . " %i %-12:c")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c"))))

(use-package org-journal
  :bind
  (:map cjv/notes-map
              ("j" . #'org-journal-new-entry))
  :init
  (setq org-journal-prefix-key nil)
  :hook (org-journal-mode . writeroom-mode)
  :custom
  (org-journal-dir (expand-file-name "journal/" org-directory)))

(provide 'init-org)
