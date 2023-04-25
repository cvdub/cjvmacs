;; Garbage collection
(setq gc-cons-threshold (* 1024 1024 100))
(setq large-file-warning-threshold (* 1024 1024 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1MB

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Make title bar transparent
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Remove internal border
(add-to-list 'default-frame-alist '(internal-border-width . 0))

;; Set startup frame background color. This is replaced later once the
;; theme is activated.
(add-to-list 'default-frame-alist '(background-color . "#fbf1c7"))
