;;; cjvmacs-spacebase.el --- Spacebase config for CVJmacs  -*- lexical-binding: t; -*-

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

;; Spacebase config for CVJmacs

;;; Code:

(defvar spacebase/directory "/Users/cjv/code/projects/spacebase/")

(defvar spacebase/servers '(("production" . "ubuntu@spacebaseapp.com")
                            ("staging" . "ubuntu@test.spacebaseapp.com")
                            ("pentest" . "ubuntu@pentest.spacebaseapp.com")))

(defun spacebase/get-server-name ()
  "Select Spacebase server name."
  (completing-read "Select server: " (mapcar 'car spacebase/servers)))

(defun spacebase/get-server ()
  "Select a Spacebase server URL by name."
  (cdr (assoc (spacebase/get-server-name) spacebase/servers)))

(defun spacebase/server-deploy (prefix-arg &optional prompt-for-task)
  "Run an async deploy command for Spacebase."
  (interactive "P")
  (let* ((skip-assets prefix-arg)
         (default-directory (expand-file-name "provisioning/" spacebase/directory))

         (playbook "webservers.yml")
         (host (spacebase/get-server-name))
         (output-buffer (format "*deploy: %s*" host))
         (branch (completing-read "Select branch: " (magit-list-local-branch-names)
                                  nil t nil nil "master"))
         (extra-vars (list (format "git_branch=%s" branch)))
         (extra-vars (if skip-assets
                         (cons "skip_assets=true" extra-vars)
                       extra-vars))
         (extra-vars (string-join extra-vars " "))
         (command (format "ansible-playbook %s -i hosts/%s --extra-vars \"%s\"" playbook host extra-vars))
         (command (if prompt-for-task
                      (format "%s --start-at-task=\"%s\"" command (read-string "Start at task: "))
                    command)))
    (cjv/start-process (format "Deploy %s to %s" branch host)
                       output-buffer
                       command
                       :alert "Deploy complete!")))

(defun spacebase/server-deploy-from-task (prefix-arg)
  "Run an async deploy command for Spacebase, starting from a specific task."
  (interactive "P")
  (spacebase/server-deploy prefix-arg t))

(defun spacebase/rebuild-test-db ()
  "Regenerates Spacebase test databases."
  (interactive)
  (let* ((test-dbs '("test_spacebase_db"
                     "test_spacebase_db_gw0"
                     "test_spacebase_db_gw1"
                     "test_spacebase_db_gw2"
                     "test_spacebase_db_gw3"
                     "test_spacebase_db_gw4"
                     "test_spacebase_db_gw5"
                     "test_spacebase_db_gw6"
                     "test_spacebase_db_gw7"
                     "test_spacebase_db_gw8"
                     "test_spacebase_db_gw9"))
         (sub-commands '("dropdb -U cjv --if-exists %s"
                         "createdb -U cjv %s"
                         "psql --quiet -U cjv -d %s -c \"create extension pg_trgm\""))
         (command (cl-loop for command-string in sub-commands
                           nconc (mapcar (lambda (db) (format command-string db)) test-dbs) into command
                           finally (return (string-join command " && ")))))
    (cjv/start-process "Rebuild Spacebase test DB" "*spacebase-rebuild-test-db*" command :message "Done!")))

(defun spacebase/update-local-db ()
  "Update local Spacebase DB to match production."
  (interactive)
  (message "Updating Spacebase local database...")
  (let ((default-directory (temporary-file-directory)))
    (cjv/with-alert
     "Update complete!"
     (start-process-shell-command
      "Update Spacebase DB"
      "*spacebase-db-update*"
      "time /Users/cjv/code/projects/spacebase/provisioning/create-local-db.sh \
      $(pass spacebase/production/database-password)"))))

(defun spacebase/open-logs ()
  "Copies Spacebase server logs to local machine, then opens them."
  (interactive)
  (message "Downloading Spacebase log file...")
  (let* ((server (spacebase/get-server))
         (command (format "rsync -v %s:/var/log/spacebase/spacebase.log ~/Downloads/" server)))
    (call-process-shell-command command))
  (find-file "~/Downloads/spacebase.log"))

(defun spacebase/magit-status ()
  "Open magit status buffer for Spacebase."
  (interactive)
  (magit-status-setup-buffer spacebase/directory))

(defmacro spacebase/with-project-directory (&rest body)
  "Set `default-directory' to Spacebase project root, then run BODY."
  `(let ((default-directory spacebase/directory))
     (hack-local-variables)
     (pyvenv-workon "sb")
     ,@body
     (pyvenv-deactivate)))

(defun spacebase/run-development-server ()
  "Run the Spacebase development server."
  (interactive)
  (spacebase/with-project-directory
   (let ((kill-buffer-query-functions nil)
         (display-buffer-alist '(("*" (display-buffer-no-window))))
         (commands '(("Spacebase runserver" "*Spacebase: runserver*" "python manage.py runserver")
                     ("Spacebase celery" "*Spacebase: celery_worker_0*" "python manage.py celery_worker -n 0 -Q celery")
                     ("Spacebase celery" "*Spacebase: celery_worker_1*" "python manage.py celery_worker -n 1 -Q parents")
                     ("Spacebase tailwind" "*Spacebase: tailwind*" "python manage.py tailwind start"))))
     (pcase-dolist (`(,name ,buffer ,command) commands)
       (cjv/start-process name buffer command :kill t))))
  (message "Started Spacebase development server"))

(defun spacebase/make-migrations ()
  "Run Spacebase makemigrations."
  (interactive)
  (message "Making migrations...")
  (spacebase/with-project-directory
   (cjv/start-process "Spacebase make migrations"
                      "*Spacebase: makemigrations*"
                      "python manage.py makemigrations"
                      :message "Done!")))

(defun spacebase/migrate ()
  "Run Spacebase migrate."
  (interactive)
  (message "Migrating...")
  (spacebase/with-project-directory
   (cjv/start-process "Spacebase migrate"
                      "*Spacebase: migrate*"
                      "python manage.py migrate"
                      :message "Done!")))

(defun spacebase/server-django-shell ()
  "Start a Django shell on a remote server."
  (interactive)
  (let ((default-directory (format "/ssh:%s:~/webapps/spacebase/" (spacebase/get-server)))
        (venv (expand-file-name "venv"))
        (python-shell-interpreter "venv/bin/python")
        (python-shell-interpreter-args "manage.py shell -i python"))
    (pyvenv-activate "venv")
    (run-python)
    (python-shell-switch-to-shell)))

(defvar spacebase/lyft-sftp-servers
  '(("Lyft Staging" .
     (:gpg-key-id "7046067B97E265ACA5878649064EA24D226C0AD6"
                  :destination "lyft-sftp-staging:/Spacebase-LyftLease-test/from Spacebase/"))
    ("Lyft Production" .
     (:gpg-key-id "7EE93014F52B58740240F2D21AA46806B17E2129"
                  :destination "lyft-sftp-production:/Spacebase-LyftLease-prod/from Spacebase/"))))

(defun spacebase/post-lyft-file (file gpg-key-id destination)
  "Encrypt and post FILE to Lyft's SFTP server (DESTINATION) using GPG-KEY-ID."
  (call-process-shell-command
   (format "gpg --recipient %s --trust-model always --armor --encrypt %s" gpg-key-id file))
  (cjv/post-to-sftp (format "%s.asc" file) destination))

(defun spacebase/post-lyft-files ()
  "Encrypt and post files marked in Dired to Lyft's SFTP server."
  (interactive)
  (let* ((server-key (completing-read "Select server: " spacebase/lyft-sftp-servers))
         (server (cdr (assoc server-key spacebase/lyft-sftp-servers)))
         (gpg-key-id (plist-get server :gpg-key-id))
         (destination (plist-get server :destination)))
    (mapc (lambda (file) (spacebase/post-lyft-file file gpg-key-id destination))
          (dired-get-marked-files))))

(defun spacebase/print-latest-commit ()
  "Print latest Spacebase commit to echo area."
  (interactive)
  (let* ((server (spacebase/get-server))
         (command (format "ssh %s 'cd webapps/spacebase && git log -1 --date=short --pretty=format:\"%%h %%ad (%%an) %%s\"'" server))
         (result (shell-command-to-string command)))
    (message "%s: %s" server result)))

;;;; Keybindings
(defvar cjv/spacebase-map (make-sparse-keymap)
  "Keymap for Spacebase commands.")

(bind-key (kbd "s") cjv/spacebase-map cjv/my-map)
(bind-key (kbd "d") #'spacebase/server-deploy cjv/spacebase-map)
(bind-key (kbd "D") #'spacebase/server-deploy-from-task cjv/spacebase-map)
(bind-key (kbd "u") #'spacebase/update-local-db cjv/spacebase-map)
(bind-key (kbd "t") #'spacebase/rebuild-test-db cjv/spacebase-map)
(bind-key (kbd "l") #'spacebase/open-logs cjv/spacebase-map)
(bind-key (kbd "r") #'spacebase/run-development-server cjv/spacebase-map)
(bind-key (kbd "M") #'spacebase/make-migrations cjv/spacebase-map)
(bind-key (kbd "m") #'spacebase/migrate cjv/spacebase-map)
(bind-key (kbd "j") #'spacebase/server-django-shell cjv/spacebase-map)
(bind-key (kbd "g") #'spacebase/print-latest-commit cjv/spacebase-map)
(global-set-key (kbd "<f12>") #'spacebase/magit-status)

(provide 'cjvmacs-spacebase)

;;; cjvmacs-spacebase.el ends here
