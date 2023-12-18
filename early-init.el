;; Garbage collection
(setq gc-cons-threshold most-positive-fixnum)
(setq large-file-warning-threshold (* 1024 1024 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1MB

;; Disable package.el
(setq package-enable-at-startup nil)

;; Make title bar transparent
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Remove internal border
(add-to-list 'default-frame-alist '(internal-border-width . 0))

;; Fix bright flash when opening Emacs
(setq mode-line-format nil)
(set-face-attribute 'default 'nil :background "#fbf1c7")
(add-hook 'after-make-frame-functions (lambda (_)
                                        (when-let ((theme (car custom-enabled-themes)))
                                          (enable-theme theme))))
