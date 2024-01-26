;; -*- lexical-binding: t; -*-

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

(defmacro cjv/with-bottom-window (&rest body)
  "Open buffer created by BODY in bottom window."
  `(let ((display-buffer-alist '(("*"
                                  (display-buffer-in-side-window)
                                  (inhibit-same-window . t)
                                  (window-height . 0.3)
                                  (side . bottom)))))
     ,@body))

(defun cjv/post-to-sftp (file dest)
  "Put FILE to SFTP server located at DEST."
  (shell-command
   (format "sftp \"%s\" <<< $\'put \"%s\"\'" dest file)))

(provide 'init-utils)
