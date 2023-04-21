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

(provide 'init-spacebase)
