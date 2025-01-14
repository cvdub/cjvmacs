(setq gc-cons-threshold most-positive-fixnum)
(setq large-file-warning-threshold (* 1024 1024 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(add-hook 'after-init-hook (lambda ()
                             (message "Resetting GC threshold.")
                             (setq gc-cons-threshold (* 16 1024 1024))))
