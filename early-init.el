;; Garbage collection
(setq gc-cons-threshold 50000000) ; 50MB
(setq large-file-warning-threshold 100000000)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Disable title bar
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Set startup frame background color. This is replaced later once the
;; theme is activated.
(add-to-list 'default-frame-alist '(background-color . "#fbf1c7"))
