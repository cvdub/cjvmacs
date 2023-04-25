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
  (let* ((default-directory "/Users/cjv/code/projects/spacebase/provisioning/")
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

(defun spacebase/open-logs ()
  "Copies Spacebase server logs to local machine, then opens them."
  (interactive)
  (message "Downloading Spacebase log file...")
  (let* ((server (spacebase/get-server))
         (command (format "rsync -v %s:/var/log/spacebase/spacebase.log ~/Downloads/" server)))
    (call-process-shell-command command))
  (find-file "~/Downloads/spacebase.log"))

(defvar cjv/spacebase-map (make-sparse-keymap)
  "Keymap for Spacebase commands.")

(bind-key (kbd "s") cjv/spacebase-map cjv/my-map)
(bind-key (kbd "d") #'spacebase/server-deploy cjv/spacebase-map)
(bind-key (kbd "t") #'spacebase/rebuild-test-db cjv/spacebase-map)
(bind-key (kbd "l") #'spacebase/open-logs cjv/spacebase-map)

(defun spacebase/magit-status ()
  "Open magit status buffer for Spacebase."
  (interactive)
  (magit-status-setup-buffer "~/code/projects/spacebase/"))

(global-set-key (kbd "<f12>") #'spacebase/magit-status)

(provide 'init-spacebase)
