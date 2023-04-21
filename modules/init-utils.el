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

(provide 'init-utils)
