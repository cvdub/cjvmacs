;; Garbage collection
(setq gc-cons-threshold most-positive-fixnum)
(setq large-file-warning-threshold (* 1024 1024 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1MB

;; Disable package.el
(setq package-enable-at-startup nil)
