(defvar spacebase/directory "/Users/cjv/code/projects/spacebase/")

(defvar spacebase/servers '(("production" . "ubuntu@spacebaseapp.com")
                            ("staging" . "ubuntu@test.spacebaseapp.com")))

(defun spacebase/get-server-name ()
  "Select Spacebase server name."
  (completing-read "Select server: " (mapcar 'car spacebase/servers)))

(defun spacebase/get-server ()
  "Select a Spacebase server URL by name."
  (cdr (assoc (spacebase/get-server-name) spacebase/servers)))

(defun spacebase/server-deploy ()
  "Runs an async deploy command for Spacebase."
  (interactive)
  (let* ((default-directory (expand-file-name "provisioning/" spacebase/directory))
         (playbook "webservers.yml")
         (host (spacebase/get-server-name))
         (output-buffer (format "*deploy: %s*" host))
         (branch (completing-read "Select branch: " (magit-list-local-branch-names)
                                  nil t nil nil "master"))
         (command (format "ansible-playbook %s -i hosts/%s --extra-vars \"git_branch=%s\""
                          playbook host branch)))
    (message "Deploying to %s to %s" branch host)
    (cjv/with-alert
     "Deploy complete!"
     (start-process-shell-command (format "Deploy %s to %s" branch host)
                                  output-buffer
                                  command))))

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
    (message "Rebuilding Spacebase test DB...")
    (cjv/with-message
     "Done!"
     (start-process-shell-command "Rebuild Spacebase test DB" "*spacebase-rebuild-test-db*" command))))

(defun spacebase/update-local-db ()
  "Updates local Spacebase DB to match production."
  (interactive)
  (message "Updating Spacebase local database...")
  (let ((default-directory (temporary-file-directory)))
    (cjv/with-alert
     "Update complete!"
     (start-process-shell-command
      "Update Spacebase DB"
      "*spacebase-db-update*"
      "/Users/cjv/code/projects/spacebase/provisioning/create-local-db.sh \
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

(defun spacebase/run-development-server ()
  "Runs the Spacebase development server."
  (interactive)
  (let ((default-directory spacebase/directory)
        (async-shell-command-buffer 'confirm-kill-process)
        (display-buffer-alist '(("*" (display-buffer-no-window)))))
    (async-shell-command "python manage.py runserver" "*Spacebase: runserver*")
    (async-shell-command "python manage.py celery_worker" "*Spacebase: celery_worker*")))

(defun spacebase/make-migrations ()
  "Runs Spacebase makemigrations."
  (interactive)
  (let ((default-directory spacebase/directory)
        (async-shell-command-buffer 'confirm-kill-process))
    (async-shell-command "python manage.py makemigrations" "*Spacebase: makemigrations*")))

(defun spacebase/migrate ()
  "Runs Spacebase migrate."
  (interactive)
  (let ((default-directory spacebase/directory)
        (async-shell-command-buffer 'confirm-kill-process))
    (async-shell-command "python manage.py migrate" "*Spacebase: migrate*")))

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

;;;; Keybindings
(defvar cjv/spacebase-map (make-sparse-keymap)
  "Keymap for Spacebase commands.")

(bind-key (kbd "s") cjv/spacebase-map cjv/my-map)
(bind-key (kbd "d") #'spacebase/server-deploy cjv/spacebase-map)
(bind-key (kbd "u") #'spacebase/update-local-db cjv/spacebase-map)
(bind-key (kbd "t") #'spacebase/rebuild-test-db cjv/spacebase-map)
(bind-key (kbd "l") #'spacebase/open-logs cjv/spacebase-map)
(bind-key (kbd "r") #'spacebase/run-development-server cjv/spacebase-map)
(bind-key (kbd "M") #'spacebase/make-migrations cjv/spacebase-map)
(bind-key (kbd "m") #'spacebase/migrate cjv/spacebase-map)
(bind-key (kbd "j") #'spacebase/server-django-shell cjv/spacebase-map)
(global-set-key (kbd "<f12>") #'spacebase/magit-status)



(provide 'init-spacebase)
