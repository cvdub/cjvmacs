;; -*- lexical-binding: t; -*-

;;;; Processes
(cl-defun cjv/alert (title body &optional (sound "default"))
  "Sends a MacOS alert."
  (start-process-shell-command
   (format "Alert %s" (gensym))
   nil ;; No output buffer
   (format "%s -title \"%s\" -message \"%s\" -sound %s"
           "/opt/homebrew/bin/alerter"
           title
           body
           sound)))

(defmacro cjv/with-alert (message-text &rest body)
  "Sends an alert after the wrapped process finishes."
  `(set-process-sentinel
    ,@body
    (lambda (p e)
      (if (= 0 (process-exit-status p))
          (cjv/alert (process-name p) ,message-text)
        (cjv/alert (process-name p) "Failed!")))))

(defun cjv/add-alert-to-process (process message-text)
  (set-process-sentinel process
                        (lambda (p e)
                          (if (= 0 (process-exit-status p))
                              (cjv/alert (process-name p) message-text)
                            (cjv/alert (process-name p) "Failed!"))))
  process)

(defmacro cjv/with-message (message-text &rest body)
  "Messages the user after process finishes."
  `(set-process-sentinel
    ,@body
    (lambda (p e)
      (if (= 0 (process-exit-status p))
          (message "%s: %s" (process-name p) ,message-text)
        (message "%s Failed!" (process-name p))))))

(defun cjv/add-message-to-process (process message-text)
  (set-process-sentinel process
                        (lambda (p e)
                          (if (= 0 (process-exit-status p))
                              (message "%s: %s" (process-name p) message-text)
                            (message "%s Failed!" (process-name p)))))
  process)

(defun cjv/add-ansi-color-to-process-buffer (process)
  (with-current-buffer (process-buffer process)
    (ansi-color-for-comint-mode-on)
    (comint-mode))
  (set-process-filter process 'comint-output-filter))

(cl-defun cjv/start-process (name buffer command &key message alert display kill)
  "Starts a process with NAME, BUFFER, and COMMAND.

Optionally, displays a MESSAGE or ALERT on completion."
  (when-let* ((buffer (get-buffer buffer))
              (process (get-buffer-process buffer))
              (delete (or kill (y-or-n-p "Kill existing process? "))))
    (delete-process process))
  (let ((process (start-process-shell-command name buffer command)))
    (when display
      (display-buffer buffer))
    (message "Started process: %s" name)
    (cjv/add-ansi-color-to-process-buffer process)
    (when message
      (cjv/add-message-to-process process message))
    (when alert
      (cjv/add-alert-to-process process alert))
    process))

;;;; Windows
(defmacro cjv/with-bottom-window (&rest body)
  "Open buffer created by BODY in bottom window."
  `(let ((display-buffer-alist '(("*"
                                  (display-buffer-in-side-window)
                                  (inhibit-same-window . t)
                                  (window-height . 0.3)
                                  (side . bottom)))))
     ,@body))


(defmacro cjv/with-same-window (&rest body)
  "Ensure buffer opened by BODY opens in the current window."
  `(let ((display-buffer-overriding-action
          '(display-buffer-same-window (inhibit-same-window . nil))))
     ,@body))


(defun cjv/post-to-sftp (file dest)
  "Put FILE to SFTP server located at DEST."
  (shell-command
   (format "sftp \"%s\" <<< $\'put \"%s\"\'" dest file)))

(defun cjv/append-date-to-filename (filename)
  "Append the current date in YYYY-MM-DD format to FILENAME."
  (let* ((current-date (format-time-string "%Y-%m-%d"))
         (base (file-name-sans-extension filename))
         (ext (file-name-extension filename))
         (ext (if ext (concat "." ext) "")))
    (format "%s-%s%s" base current-date ext)))

(provide 'init-utils)
